!   ***********************************************************************************************
!   **                                                                                           **
!   **                  |\   -  -.   ./                                                          **
!   **                  | \./ \/ | ./ /     __________  ___________ __________                   **
!   **                __|         /  /     / ____/ __ \/ ____/ ___// ____/ __ \                  **
!   **                \    .        /.-/  / /_  / /_/ / __/  \__ \/ /   / / / /                  **
!   **                 \   |\.|\/|    /  / __/ / _, _/ /___ ___/ / /___/ /_/ /                   **
!   **                  \__\     /___/  /_/   /_/ |_/_____//____/\____/\____/                    **
!   **                                                                                           **
!   **                                     FRESCO-SpeedCHEM                                      **
!   **            A code for internal combustion engine flows with chemical kinetics             **
!   **                                                                                           **
!   ***********************************************************************************************
!   **                                                         **
!   **    fast_float_module                                     **
!> @brief Eisel-Lemire fast float parsing (ffc.h port)         **
!   **                                                         **
!   *************************************************************
!> @author Federico Perini <federico.perini@gmail.com>
!> @since     Mon, 10 Mar 2026
!   *************************************************************
module fast_float_module
    use iso_fortran_env, only: int8, int32, int64, real32, real64
    use iso_c_binding, only: c_int128_t
    use ieee_arithmetic
    implicit none(type, external)
    private

    public :: ffc_parse_double, ffc_parse_double_range
    public :: ffc_parse_double_range_sub
    public :: ffc_parse_float
    public :: ffc_parse_i64, ffc_parse_i32
    public :: ffc_parse_options, ffc_result
    public :: FFC_OUTCOME_OK, FFC_OUTCOME_INVALID_INPUT
    public :: FFC_OUTCOME_OUT_OF_RANGE
    public :: FFC_PRESET_GENERAL, FFC_PRESET_JSON
    public :: FFC_PRESET_FORTRAN, DEFAULT_PARSING

    interface ffc_parse_double
        module procedure ffc_pd, ffc_pd_opts
    end interface ffc_parse_double

    interface ffc_parse_double_range
        module procedure ffc_pdr, ffc_pdr_opts
    end interface ffc_parse_double_range

    interface ffc_parse_float
        module procedure ffc_pf, ffc_pf_opts
    end interface ffc_parse_float

    integer(int8), parameter :: FFC_OUTCOME_OK            = 0
    integer(int8), parameter :: FFC_OUTCOME_INVALID_INPUT = 1
    integer(int8), parameter :: FFC_OUTCOME_OUT_OF_RANGE  = 2

    integer(int64), parameter :: FMT_SCI  = ishft(1_int64, 0)
    integer(int64), parameter :: FMT_FIX  = ishft(1_int64, 2)
    integer(int64), parameter :: FMT_NOIN = ishft(1_int64, 4)
    integer(int64), parameter :: FMT_JSON = ishft(1_int64, 5)
    integer(int64), parameter :: FMT_FORT = ishft(1_int64, 6)
    integer(int64), parameter :: FMT_PLUS = ishft(1_int64, 7)
    integer(int64), parameter :: FMT_SKIP = ishft(1_int64, 8)

    integer(int64), parameter :: FFC_PRESET_GENERAL = &
        ior(FMT_FIX, FMT_SCI)
    integer(int64), parameter :: FFC_PRESET_JSON = &
        ior(ior(FMT_JSON,FFC_PRESET_GENERAL),FMT_NOIN)
    integer(int64), parameter :: FFC_PRESET_FORTRAN = &
        ior(FMT_FORT, FFC_PRESET_GENERAL)

    integer(int32), parameter :: INVALID_AM = -32768_int32
    integer(int64), parameter :: SB64 = ishft(1_int64, 63)
    integer(int64), parameter :: M32 = int(z'00000000FFFFFFFF', int64)

    ! 128-bit integer support: compile-time detection
    logical, parameter :: HAS_INT128 = c_int128_t > 0
    integer, parameter :: IK128 = merge(c_int128_t, int64, HAS_INT128)

    ! Compile-time endianness detection
    integer(int32), parameter :: ENDIAN_TAG = transfer([1_int8, 0_int8, 0_int8, 0_int8], 0_int32)
    logical, parameter :: LITTLE_ENDIAN = ENDIAN_TAG == 1_int32
        
    integer, private :: i

    type :: ffc_result
        integer :: pos 
        integer(int8) :: outcome 
    end type ffc_result

    type :: ffc_parse_options
        integer(int64) :: format = FFC_PRESET_GENERAL
        character :: decimal_point = '.'
    end type ffc_parse_options
    
    type(ffc_parse_options), parameter :: DEFAULT_PARSING = ffc_parse_options(FFC_PRESET_GENERAL,'.')

    type :: u128
        integer(int64) :: lo = 0_int64
        integer(int64) :: hi = 0_int64
    end type u128

    type :: ifmt
        integer :: meb, mine, infp, sidx
        integer :: minrte, maxrte, minfp, maxfp
        integer(int64) :: maxm, emask, mmask, hbm
        integer :: smp10, lgp10, maxd
    end type ifmt

    ! Double precision format parameters
    type(ifmt), parameter :: DF = ifmt( &
        meb=52, mine=-1023, infp=2047, sidx=63, &
        minrte=-4, maxrte=23, minfp=-22, maxfp=22, &
        maxm =ishft(2_int64, 52), &
        emask=int(z'7FF0000000000000', int64), &
        mmask=int(z'000FFFFFFFFFFFFF', int64), &
        hbm  =int(z'0010000000000000', int64), &
        smp10=-342, lgp10=308, maxd=769)

    ! Float format parameters
    type(ifmt), parameter :: FF = ifmt( &
        meb=23, mine=-127, infp=255, sidx=31, &
        minrte=-17, maxrte=10, minfp=-10, maxfp=10, &
        maxm=ishft(2_int64, 23), &
        emask=int(z'000000007F800000', int64), &
        mmask=int(z'00000000007FFFFF', int64), &
        hbm=int(z'0000000000800000', int64), &
        smp10=-64, lgp10=38, maxd=114)

    type :: fparsed
        integer(int64) :: exponent = 0
        integer(int64) :: mantissa = 0
        integer :: lastm = 0
        logical :: neg = .false.
        logical :: valid = .false.
        logical :: tmd = .false.
        integer :: ips = 0, ipl = 0
        integer :: fps = 0, fpl = 0
    end type fparsed

    type :: fam
        integer(int64) :: mantissa = 0
        integer(int32) :: power2 = 0
    end type fam

    integer, parameter :: LB = 64
    integer, parameter :: BBITS = 4000
    integer, parameter :: SVLC = BBITS / LB

    type :: fsv
        integer(int64) :: d(SVLC) = 0_int64
        integer :: ln = 0
    end type fsv

    type :: fbigint
        type(fsv) :: vec
    end type fbigint

    integer, parameter :: P5CNT = 1302
    integer(int64), parameter :: P5_1(200) = [ &
        int(z'eef453d6923bd65a', int64), &
        int(z'113faa2906a13b3f', int64), &
        int(z'9558b4661b6565f8', int64), &
        int(z'4ac7ca59a424c507', int64), &
        int(z'baaee17fa23ebf76', int64), &
        int(z'5d79bcf00d2df649', int64), &
        int(z'e95a99df8ace6f53', int64), &
        int(z'f4d82c2c107973dc', int64), &
        int(z'91d8a02bb6c10594', int64), &
        int(z'79071b9b8a4be869', int64), &
        int(z'b64ec836a47146f9', int64), &
        int(z'9748e2826cdee284', int64), &
        int(z'e3e27a444d8d98b7', int64), &
        int(z'fd1b1b2308169b25', int64), &
        int(z'8e6d8c6ab0787f72', int64), &
        int(z'fe30f0f5e50e20f7', int64), &
        int(z'b208ef855c969f4f', int64), &
        int(z'bdbd2d335e51a935', int64), &
        int(z'de8b2b66b3bc4723', int64), &
        int(z'ad2c788035e61382', int64), &
        int(z'8b16fb203055ac76', int64), &
        int(z'4c3bcb5021afcc31', int64), &
        int(z'addcb9e83c6b1793', int64), &
        int(z'df4abe242a1bbf3d', int64), &
        int(z'd953e8624b85dd78', int64), &
        int(z'd71d6dad34a2af0d', int64), &
        int(z'87d4713d6f33aa6b', int64), &
        int(z'8672648c40e5ad68', int64), &
        int(z'a9c98d8ccb009506', int64), &
        int(z'680efdaf511f18c2', int64), &
        int(z'd43bf0effdc0ba48', int64), &
        int(z'212bd1b2566def2', int64), &
        int(z'84a57695fe98746d', int64), &
        int(z'14bb630f7604b57', int64), &
        int(z'a5ced43b7e3e9188', int64), &
        int(z'419ea3bd35385e2d', int64), &
        int(z'cf42894a5dce35ea', int64), &
        int(z'52064cac828675b9', int64), &
        int(z'818995ce7aa0e1b2', int64), &
        int(z'7343efebd1940993', int64), &
        int(z'a1ebfb4219491a1f', int64), &
        int(z'1014ebe6c5f90bf8', int64), &
        int(z'ca66fa129f9b60a6', int64), &
        int(z'd41a26e077774ef6', int64), &
        int(z'fd00b897478238d0', int64), &
        int(z'8920b098955522b4', int64), &
        int(z'9e20735e8cb16382', int64), &
        int(z'55b46e5f5d5535b0', int64), &
        int(z'c5a890362fddbc62', int64), &
        int(z'eb2189f734aa831d', int64), &
        int(z'f712b443bbd52b7b', int64), &
        int(z'a5e9ec7501d523e4', int64), &
        int(z'9a6bb0aa55653b2d', int64), &
        int(z'47b233c92125366e', int64), &
        int(z'c1069cd4eabe89f8', int64), &
        int(z'999ec0bb696e840a', int64), &
        int(z'f148440a256e2c76', int64), &
        int(z'c00670ea43ca250d', int64), &
        int(z'96cd2a865764dbca', int64), &
        int(z'380406926a5e5728', int64), &
        int(z'bc807527ed3e12bc', int64), &
        int(z'c605083704f5ecf2', int64), &
        int(z'eba09271e88d976b', int64), &
        int(z'f7864a44c633682e', int64), &
        int(z'93445b8731587ea3', int64), &
        int(z'7ab3ee6afbe0211d', int64), &
        int(z'b8157268fdae9e4c', int64), &
        int(z'5960ea05bad82964', int64), &
        int(z'e61acf033d1a45df', int64), &
        int(z'6fb92487298e33bd', int64), &
        int(z'8fd0c16206306bab', int64), &
        int(z'a5d3b6d479f8e056', int64), &
        int(z'b3c4f1ba87bc8696', int64), &
        int(z'8f48a4899877186c', int64), &
        int(z'e0b62e2929aba83c', int64), &
        int(z'331acdabfe94de87', int64), &
        int(z'8c71dcd9ba0b4925', int64), &
        int(z'9ff0c08b7f1d0b14', int64), &
        int(z'af8e5410288e1b6f', int64), &
        int(z'7ecf0ae5ee44dd9', int64), &
        int(z'db71e91432b1a24a', int64), &
        int(z'c9e82cd9f69d6150', int64), &
        int(z'892731ac9faf056e', int64), &
        int(z'be311c083a225cd2', int64), &
        int(z'ab70fe17c79ac6ca', int64), &
        int(z'6dbd630a48aaf406', int64), &
        int(z'd64d3d9db981787d', int64), &
        int(z'92cbbccdad5b108', int64), &
        int(z'85f0468293f0eb4e', int64), &
        int(z'25bbf56008c58ea5', int64), &
        int(z'a76c582338ed2621', int64), &
        int(z'af2af2b80af6f24e', int64), &
        int(z'd1476e2c07286faa', int64), &
        int(z'1af5af660db4aee1', int64), &
        int(z'82cca4db847945ca', int64), &
        int(z'50d98d9fc890ed4d', int64), &
        int(z'a37fce126597973c', int64), &
        int(z'e50ff107bab528a0', int64), &
        int(z'cc5fc196fefd7d0c', int64), &
        int(z'1e53ed49a96272c8', int64), &
        int(z'ff77b1fcbebcdc4f', int64), &
        int(z'25e8e89c13bb0f7a', int64), &
        int(z'9faacf3df73609b1', int64), &
        int(z'77b191618c54e9ac', int64), &
        int(z'c795830d75038c1d', int64), &
        int(z'd59df5b9ef6a2417', int64), &
        int(z'f97ae3d0d2446f25', int64), &
        int(z'4b0573286b44ad1d', int64), &
        int(z'9becce62836ac577', int64), &
        int(z'4ee367f9430aec32', int64), &
        int(z'c2e801fb244576d5', int64), &
        int(z'229c41f793cda73f', int64), &
        int(z'f3a20279ed56d48a', int64), &
        int(z'6b43527578c1110f', int64), &
        int(z'9845418c345644d6', int64), &
        int(z'830a13896b78aaa9', int64), &
        int(z'be5691ef416bd60c', int64), &
        int(z'23cc986bc656d553', int64), &
        int(z'edec366b11c6cb8f', int64), &
        int(z'2cbfbe86b7ec8aa8', int64), &
        int(z'94b3a202eb1c3f39', int64), &
        int(z'7bf7d71432f3d6a9', int64), &
        int(z'b9e08a83a5e34f07', int64), &
        int(z'daf5ccd93fb0cc53', int64), &
        int(z'e858ad248f5c22c9', int64), &
        int(z'd1b3400f8f9cff68', int64), &
        int(z'91376c36d99995be', int64), &
        int(z'23100809b9c21fa1', int64), &
        int(z'b58547448ffffb2d', int64), &
        int(z'abd40a0c2832a78a', int64), &
        int(z'e2e69915b3fff9f9', int64), &
        int(z'16c90c8f323f516c', int64), &
        int(z'8dd01fad907ffc3b', int64), &
        int(z'ae3da7d97f6792e3', int64), &
        int(z'b1442798f49ffb4a', int64), &
        int(z'99cd11cfdf41779c', int64), &
        int(z'dd95317f31c7fa1d', int64), &
        int(z'40405643d711d583', int64), &
        int(z'8a7d3eef7f1cfc52', int64), &
        int(z'482835ea666b2572', int64), &
        int(z'ad1c8eab5ee43b66', int64), &
        int(z'da3243650005eecf', int64), &
        int(z'd863b256369d4a40', int64), &
        int(z'90bed43e40076a82', int64), &
        int(z'873e4f75e2224e68', int64), &
        int(z'5a7744a6e804a291', int64), &
        int(z'a90de3535aaae202', int64), &
        int(z'711515d0a205cb36', int64), &
        int(z'd3515c2831559a83', int64), &
        int(z'd5a5b44ca873e03', int64), &
        int(z'8412d9991ed58091', int64), &
        int(z'e858790afe9486c2', int64), &
        int(z'a5178fff668ae0b6', int64), &
        int(z'626e974dbe39a872', int64), &
        int(z'ce5d73ff402d98e3', int64), &
        int(z'fb0a3d212dc8128f', int64), &
        int(z'80fa687f881c7f8e', int64), &
        int(z'7ce66634bc9d0b99', int64), &
        int(z'a139029f6a239f72', int64), &
        int(z'1c1fffc1ebc44e80', int64), &
        int(z'c987434744ac874e', int64), &
        int(z'a327ffb266b56220', int64), &
        int(z'fbe9141915d7a922', int64), &
        int(z'4bf1ff9f0062baa8', int64), &
        int(z'9d71ac8fada6c9b5', int64), &
        int(z'6f773fc3603db4a9', int64), &
        int(z'c4ce17b399107c22', int64), &
        int(z'cb550fb4384d21d3', int64), &
        int(z'f6019da07f549b2b', int64), &
        int(z'7e2a53a146606a48', int64), &
        int(z'99c102844f94e0fb', int64), &
        int(z'2eda7444cbfc426d', int64), &
        int(z'c0314325637a1939', int64), &
        int(z'fa911155fefb5308', int64), &
        int(z'f03d93eebc589f88', int64), &
        int(z'793555ab7eba27ca', int64), &
        int(z'96267c7535b763b5', int64), &
        int(z'4bc1558b2f3458de', int64), &
        int(z'bbb01b9283253ca2', int64), &
        int(z'9eb1aaedfb016f16', int64), &
        int(z'ea9c227723ee8bcb', int64), &
        int(z'465e15a979c1cadc', int64), &
        int(z'92a1958a7675175f', int64), &
        int(z'bfacd89ec191ec9', int64), &
        int(z'b749faed14125d36', int64), &
        int(z'cef980ec671f667b', int64), &
        int(z'e51c79a85916f484', int64), &
        int(z'82b7e12780e7401a', int64), &
        int(z'8f31cc0937ae58d2', int64), &
        int(z'd1b2ecb8b0908810', int64), &
        int(z'b2fe3f0b8599ef07', int64), &
        int(z'861fa7e6dcb4aa15', int64), &
        int(z'dfbdcece67006ac9', int64), &
        int(z'67a791e093e1d49a', int64), &
        int(z'8bd6a141006042bd', int64), &
        int(z'e0c8bb2c5c6d24e0', int64), &
        int(z'aecc49914078536d', int64), &
        int(z'58fae9f773886e18', int64), &
        int(z'da7f5bf590966848', int64), &
        int(z'af39a475506a899e', int64) ]
    integer(int64), parameter :: P5_2(200) = [ &
        int(z'888f99797a5e012d', int64), &
        int(z'6d8406c952429603', int64), &
        int(z'aab37fd7d8f58178', int64), &
        int(z'c8e5087ba6d33b83', int64), &
        int(z'd5605fcdcf32e1d6', int64), &
        int(z'fb1e4a9a90880a64', int64), &
        int(z'855c3be0a17fcd26', int64), &
        int(z'5cf2eea09a55067f', int64), &
        int(z'a6b34ad8c9dfc06f', int64), &
        int(z'f42faa48c0ea481e', int64), &
        int(z'd0601d8efc57b08b', int64), &
        int(z'f13b94daf124da26', int64), &
        int(z'823c12795db6ce57', int64), &
        int(z'76c53d08d6b70858', int64), &
        int(z'a2cb1717b52481ed', int64), &
        int(z'54768c4b0c64ca6e', int64), &
        int(z'cb7ddcdda26da268', int64), &
        int(z'a9942f5dcf7dfd09', int64), &
        int(z'fe5d54150b090b02', int64), &
        int(z'd3f93b35435d7c4c', int64), &
        int(z'9efa548d26e5a6e1', int64), &
        int(z'c47bc5014a1a6daf', int64), &
        int(z'c6b8e9b0709f109a', int64), &
        int(z'359ab6419ca1091b', int64), &
        int(z'f867241c8cc6d4c0', int64), &
        int(z'c30163d203c94b62', int64), &
        int(z'9b407691d7fc44f8', int64), &
        int(z'79e0de63425dcf1d', int64), &
        int(z'c21094364dfb5636', int64), &
        int(z'985915fc12f542e4', int64), &
        int(z'f294b943e17a2bc4', int64), &
        int(z'3e6f5b7b17b2939d', int64), &
        int(z'979cf3ca6cec5b5a', int64), &
        int(z'a705992ceecf9c42', int64), &
        int(z'bd8430bd08277231', int64), &
        int(z'50c6ff782a838353', int64), &
        int(z'ece53cec4a314ebd', int64), &
        int(z'a4f8bf5635246428', int64), &
        int(z'940f4613ae5ed136', int64), &
        int(z'871b7795e136be99', int64), &
        int(z'b913179899f68584', int64), &
        int(z'28e2557b59846e3f', int64), &
        int(z'e757dd7ec07426e5', int64), &
        int(z'331aeada2fe589cf', int64), &
        int(z'9096ea6f3848984f', int64), &
        int(z'3ff0d2c85def7621', int64), &
        int(z'b4bca50b065abe63', int64), &
        int(z'fed077a756b53a9', int64), &
        int(z'e1ebce4dc7f16dfb', int64), &
        int(z'd3e8495912c62894', int64), &
        int(z'8d3360f09cf6e4bd', int64), &
        int(z'64712dd7abbbd95c', int64), &
        int(z'b080392cc4349dec', int64), &
        int(z'bd8d794d96aacfb3', int64), &
        int(z'dca04777f541c567', int64), &
        int(z'ecf0d7a0fc5583a0', int64), &
        int(z'89e42caaf9491b60', int64), &
        int(z'f41686c49db57244', int64), &
        int(z'ac5d37d5b79b6239', int64), &
        int(z'311c2875c522ced5', int64), &
        int(z'd77485cb25823ac7', int64), &
        int(z'7d633293366b828b', int64), &
        int(z'86a8d39ef77164bc', int64), &
        int(z'ae5dff9c02033197', int64), &
        int(z'a8530886b54dbdeb', int64), &
        int(z'd9f57f830283fdfc', int64), &
        int(z'd267caa862a12d66', int64), &
        int(z'd072df63c324fd7b', int64), &
        int(z'8380dea93da4bc60', int64), &
        int(z'4247cb9e59f71e6d', int64), &
        int(z'a46116538d0deb78', int64), &
        int(z'52d9be85f074e608', int64), &
        int(z'cd795be870516656', int64), &
        int(z'67902e276c921f8b', int64), &
        int(z'806bd9714632dff6', int64), &
        int(z'ba1cd8a3db53b6', int64), &
        int(z'a086cfcd97bf97f3', int64), &
        int(z'80e8a40eccd228a4', int64), &
        int(z'c8a883c0fdaf7df0', int64), &
        int(z'6122cd128006b2cd', int64), &
        int(z'fad2a4b13d1b5d6c', int64), &
        int(z'796b805720085f81', int64), &
        int(z'9cc3a6eec6311a63', int64), &
        int(z'cbe3303674053bb0', int64), &
        int(z'c3f490aa77bd60fc', int64), &
        int(z'bedbfc4411068a9c', int64), &
        int(z'f4f1b4d515acb93b', int64), &
        int(z'ee92fb5515482d44', int64), &
        int(z'991711052d8bf3c5', int64), &
        int(z'751bdd152d4d1c4a', int64), &
        int(z'bf5cd54678eef0b6', int64), &
        int(z'd262d45a78a0635d', int64), &
        int(z'ef340a98172aace4', int64), &
        int(z'86fb897116c87c34', int64), &
        int(z'9580869f0e7aac0e', int64), &
        int(z'd45d35e6ae3d4da0', int64), &
        int(z'bae0a846d2195712', int64), &
        int(z'8974836059cca109', int64), &
        int(z'e998d258869facd7', int64), &
        int(z'2bd1a438703fc94b', int64), &
        int(z'91ff83775423cc06', int64), &
        int(z'7b6306a34627ddcf', int64), &
        int(z'b67f6455292cbf08', int64), &
        int(z'1a3bc84c17b1d542', int64), &
        int(z'e41f3d6a7377eeca', int64), &
        int(z'20caba5f1d9e4a93', int64), &
        int(z'8e938662882af53e', int64), &
        int(z'547eb47b7282ee9c', int64), &
        int(z'b23867fb2a35b28d', int64), &
        int(z'e99e619a4f23aa43', int64), &
        int(z'dec681f9f4c31f31', int64), &
        int(z'6405fa00e2ec94d4', int64), &
        int(z'8b3c113c38f9f37e', int64), &
        int(z'de83bc408dd3dd04', int64), &
        int(z'ae0b158b4738705e', int64), &
        int(z'9624ab50b148d445', int64), &
        int(z'd98ddaee19068c76', int64), &
        int(z'3badd624dd9b0957', int64), &
        int(z'87f8a8d4cfa417c9', int64), &
        int(z'e54ca5d70a80e5d6', int64), &
        int(z'a9f6d30a038d1dbc', int64), &
        int(z'5e9fcf4ccd211f4c', int64), &
        int(z'd47487cc8470652b', int64), &
        int(z'7647c3200069671f', int64), &
        int(z'84c8d4dfd2c63f3b', int64), &
        int(z'29ecd9f40041e073', int64), &
        int(z'a5fb0a17c777cf09', int64), &
        int(z'f468107100525890', int64), &
        int(z'cf79cc9db955c2cc', int64), &
        int(z'7182148d4066eeb4', int64), &
        int(z'81ac1fe293d599bf', int64), &
        int(z'c6f14cd848405530', int64), &
        int(z'a21727db38cb002f', int64), &
        int(z'b8ada00e5a506a7c', int64), &
        int(z'ca9cf1d206fdc03b', int64), &
        int(z'a6d90811f0e4851c', int64), &
        int(z'fd442e4688bd304a', int64), &
        int(z'908f4a166d1da663', int64), &
        int(z'9e4a9cec15763e2e', int64), &
        int(z'9a598e4e043287fe', int64), &
        int(z'c5dd44271ad3cdba', int64), &
        int(z'40eff1e1853f29fd', int64), &
        int(z'f7549530e188c128', int64), &
        int(z'd12bee59e68ef47c', int64), &
        int(z'9a94dd3e8cf578b9', int64), &
        int(z'82bb74f8301958ce', int64), &
        int(z'c13a148e3032d6e7', int64), &
        int(z'e36a52363c1faf01', int64), &
        int(z'f18899b1bc3f8ca1', int64), &
        int(z'dc44e6c3cb279ac1', int64), &
        int(z'96f5600f15a7b7e5', int64), &
        int(z'29ab103a5ef8c0b9', int64), &
        int(z'bcb2b812db11a5de', int64), &
        int(z'7415d448f6b6f0e7', int64), &
        int(z'ebdf661791d60f56', int64), &
        int(z'111b495b3464ad21', int64), &
        int(z'936b9fcebb25c995', int64), &
        int(z'cab10dd900beec34', int64), &
        int(z'b84687c269ef3bfb', int64), &
        int(z'3d5d514f40eea742', int64), &
        int(z'e65829b3046b0afa', int64), &
        int(z'cb4a5a3112a5112', int64), &
        int(z'8ff71a0fe2c2e6dc', int64), &
        int(z'47f0e785eaba72ab', int64), &
        int(z'b3f4e093db73a093', int64), &
        int(z'59ed216765690f56', int64), &
        int(z'e0f218b8d25088b8', int64), &
        int(z'306869c13ec3532c', int64), &
        int(z'8c974f7383725573', int64), &
        int(z'1e414218c73a13fb', int64), &
        int(z'afbd2350644eeacf', int64), &
        int(z'e5d1929ef90898fa', int64), &
        int(z'dbac6c247d62a583', int64), &
        int(z'df45f746b74abf39', int64), &
        int(z'894bc396ce5da772', int64), &
        int(z'6b8bba8c328eb783', int64), &
        int(z'ab9eb47c81f5114f', int64), &
        int(z'66ea92f3f326564', int64), &
        int(z'd686619ba27255a2', int64), &
        int(z'c80a537b0efefebd', int64), &
        int(z'8613fd0145877585', int64), &
        int(z'bd06742ce95f5f36', int64), &
        int(z'a798fc4196e952e7', int64), &
        int(z'2c48113823b73704', int64), &
        int(z'd17f3b51fca3a7a0', int64), &
        int(z'f75a15862ca504c5', int64), &
        int(z'82ef85133de648c4', int64), &
        int(z'9a984d73dbe722fb', int64), &
        int(z'a3ab66580d5fdaf5', int64), &
        int(z'c13e60d0d2e0ebba', int64), &
        int(z'cc963fee10b7d1b3', int64), &
        int(z'318df905079926a8', int64), &
        int(z'ffbbcfe994e5c61f', int64), &
        int(z'fdf17746497f7052', int64), &
        int(z'9fd561f1fd0f9bd3', int64), &
        int(z'feb6ea8bedefa633', int64), &
        int(z'c7caba6e7c5382c8', int64), &
        int(z'fe64a52ee96b8fc0', int64), &
        int(z'f9bd690a1b68637b', int64), &
        int(z'3dfdce7aa3c673b0', int64) ]
    integer(int64), parameter :: P5_3(200) = [ &
        int(z'9c1661a651213e2d', int64), &
        int(z'6bea10ca65c084e', int64), &
        int(z'c31bfa0fe5698db8', int64), &
        int(z'486e494fcff30a62', int64), &
        int(z'f3e2f893dec3f126', int64), &
        int(z'5a89dba3c3efccfa', int64), &
        int(z'986ddb5c6b3a76b7', int64), &
        int(z'f89629465a75e01c', int64), &
        int(z'be89523386091465', int64), &
        int(z'f6bbb397f1135823', int64), &
        int(z'ee2ba6c0678b597f', int64), &
        int(z'746aa07ded582e2c', int64), &
        int(z'94db483840b717ef', int64), &
        int(z'a8c2a44eb4571cdc', int64), &
        int(z'ba121a4650e4ddeb', int64), &
        int(z'92f34d62616ce413', int64), &
        int(z'e896a0d7e51e1566', int64), &
        int(z'77b020baf9c81d17', int64), &
        int(z'915e2486ef32cd60', int64), &
        int(z'ace1474dc1d122e', int64), &
        int(z'b5b5ada8aaff80b8', int64), &
        int(z'd819992132456ba', int64), &
        int(z'e3231912d5bf60e6', int64), &
        int(z'10e1fff697ed6c69', int64), &
        int(z'8df5efabc5979c8f', int64), &
        int(z'ca8d3ffa1ef463c1', int64), &
        int(z'b1736b96b6fd83b3', int64), &
        int(z'bd308ff8a6b17cb2', int64), &
        int(z'ddd0467c64bce4a0', int64), &
        int(z'ac7cb3f6d05ddbde', int64), &
        int(z'8aa22c0dbef60ee4', int64), &
        int(z'6bcdf07a423aa96b', int64), &
        int(z'ad4ab7112eb3929d', int64), &
        int(z'86c16c98d2c953c6', int64), &
        int(z'd89d64d57a607744', int64), &
        int(z'e871c7bf077ba8b7', int64), &
        int(z'87625f056c7c4a8b', int64), &
        int(z'11471cd764ad4972', int64), &
        int(z'a93af6c6c79b5d2d', int64), &
        int(z'd598e40d3dd89bcf', int64), &
        int(z'd389b47879823479', int64), &
        int(z'4aff1d108d4ec2c3', int64), &
        int(z'843610cb4bf160cb', int64), &
        int(z'cedf722a585139ba', int64), &
        int(z'a54394fe1eedb8fe', int64), &
        int(z'c2974eb4ee658828', int64), &
        int(z'ce947a3da6a9273e', int64), &
        int(z'733d226229feea32', int64), &
        int(z'811ccc668829b887', int64), &
        int(z'806357d5a3f525f', int64), &
        int(z'a163ff802a3426a8', int64), &
        int(z'ca07c2dcb0cf26f7', int64), &
        int(z'c9bcff6034c13052', int64), &
        int(z'fc89b393dd02f0b5', int64), &
        int(z'fc2c3f3841f17c67', int64), &
        int(z'bbac2078d443ace2', int64), &
        int(z'9d9ba7832936edc0', int64), &
        int(z'd54b944b84aa4c0d', int64), &
        int(z'c5029163f384a931', int64), &
        int(z'a9e795e65d4df11', int64), &
        int(z'f64335bcf065d37d', int64), &
        int(z'4d4617b5ff4a16d5', int64), &
        int(z'99ea0196163fa42e', int64), &
        int(z'504bced1bf8e4e45', int64), &
        int(z'c06481fb9bcf8d39', int64), &
        int(z'e45ec2862f71e1d6', int64), &
        int(z'f07da27a82c37088', int64), &
        int(z'5d767327bb4e5a4c', int64), &
        int(z'964e858c91ba2655', int64), &
        int(z'3a6a07f8d510f86f', int64), &
        int(z'bbe226efb628afea', int64), &
        int(z'890489f70a55368b', int64), &
        int(z'eadab0aba3b2dbe5', int64), &
        int(z'2b45ac74ccea842e', int64), &
        int(z'92c8ae6b464fc96f', int64), &
        int(z'3b0b8bc90012929d', int64), &
        int(z'b77ada0617e3bbcb', int64), &
        int(z'9ce6ebb40173744', int64), &
        int(z'e55990879ddcaabd', int64), &
        int(z'cc420a6a101d0515', int64), &
        int(z'8f57fa54c2a9eab6', int64), &
        int(z'9fa946824a12232d', int64), &
        int(z'b32df8e9f3546564', int64), &
        int(z'47939822dc96abf9', int64), &
        int(z'dff9772470297ebd', int64), &
        int(z'59787e2b93bc56f7', int64), &
        int(z'8bfbea76c619ef36', int64), &
        int(z'57eb4edb3c55b65a', int64), &
        int(z'aefae51477a06b03', int64), &
        int(z'ede622920b6b23f1', int64), &
        int(z'dab99e59958885c4', int64), &
        int(z'e95fab368e45eced', int64), &
        int(z'88b402f7fd75539b', int64), &
        int(z'11dbcb0218ebb414', int64), &
        int(z'aae103b5fcd2a881', int64), &
        int(z'd652bdc29f26a119', int64), &
        int(z'd59944a37c0752a2', int64), &
        int(z'4be76d3346f0495f', int64), &
        int(z'857fcae62d8493a5', int64), &
        int(z'6f70a4400c562ddb', int64), &
        int(z'a6dfbd9fb8e5b88e', int64), &
        int(z'cb4ccd500f6bb952', int64), &
        int(z'd097ad07a71f26b2', int64), &
        int(z'7e2000a41346a7a7', int64), &
        int(z'825ecc24c873782f', int64), &
        int(z'8ed400668c0c28c8', int64), &
        int(z'a2f67f2dfa90563b', int64), &
        int(z'728900802f0f32fa', int64), &
        int(z'cbb41ef979346bca', int64), &
        int(z'4f2b40a03ad2ffb9', int64), &
        int(z'fea126b7d78186bc', int64), &
        int(z'e2f610c84987bfa8', int64), &
        int(z'9f24b832e6b0f436', int64), &
        int(z'dd9ca7d2df4d7c9', int64), &
        int(z'c6ede63fa05d3143', int64), &
        int(z'91503d1c79720dbb', int64), &
        int(z'f8a95fcf88747d94', int64), &
        int(z'75a44c6397ce912a', int64), &
        int(z'9b69dbe1b548ce7c', int64), &
        int(z'c986afbe3ee11aba', int64), &
        int(z'c24452da229b021b', int64), &
        int(z'fbe85badce996168', int64), &
        int(z'f2d56790ab41c2a2', int64), &
        int(z'fae27299423fb9c3', int64), &
        int(z'97c560ba6b0919a5', int64), &
        int(z'dccd879fc967d41a', int64), &
        int(z'bdb6b8e905cb600f', int64), &
        int(z'5400e987bbc1c920', int64), &
        int(z'ed246723473e3813', int64), &
        int(z'290123e9aab23b68', int64), &
        int(z'9436c0760c86e30b', int64), &
        int(z'f9a0b6720aaf6521', int64), &
        int(z'b94470938fa89bce', int64), &
        int(z'f808e40e8d5b3e69', int64), &
        int(z'e7958cb87392c2c2', int64), &
        int(z'b60b1d1230b20e04', int64), &
        int(z'90bd77f3483bb9b9', int64), &
        int(z'b1c6f22b5e6f48c2', int64), &
        int(z'b4ecd5f01a4aa828', int64), &
        int(z'1e38aeb6360b1af3', int64), &
        int(z'e2280b6c20dd5232', int64), &
        int(z'25c6da63c38de1b0', int64), &
        int(z'8d590723948a535f', int64), &
        int(z'579c487e5a38ad0e', int64), &
        int(z'b0af48ec79ace837', int64), &
        int(z'2d835a9df0c6d851', int64), &
        int(z'dcdb1b2798182244', int64), &
        int(z'f8e431456cf88e65', int64), &
        int(z'8a08f0f8bf0f156b', int64), &
        int(z'1b8e9ecb641b58ff', int64), &
        int(z'ac8b2d36eed2dac5', int64), &
        int(z'e272467e3d222f3f', int64), &
        int(z'd7adf884aa879177', int64), &
        int(z'5b0ed81dcc6abb0f', int64), &
        int(z'86ccbb52ea94baea', int64), &
        int(z'98e947129fc2b4e9', int64), &
        int(z'a87fea27a539e9a5', int64), &
        int(z'3f2398d747b36224', int64), &
        int(z'd29fe4b18e88640e', int64), &
        int(z'8eec7f0d19a03aad', int64), &
        int(z'83a3eeeef9153e89', int64), &
        int(z'1953cf68300424ac', int64), &
        int(z'a48ceaaab75a8e2b', int64), &
        int(z'5fa8c3423c052dd7', int64), &
        int(z'cdb02555653131b6', int64), &
        int(z'3792f412cb06794d', int64), &
        int(z'808e17555f3ebf11', int64), &
        int(z'e2bbd88bbee40bd0', int64), &
        int(z'a0b19d2ab70e6ed6', int64), &
        int(z'5b6aceaeae9d0ec4', int64), &
        int(z'c8de047564d20a8b', int64), &
        int(z'f245825a5a445275', int64), &
        int(z'fb158592be068d2e', int64), &
        int(z'eed6e2f0f0d56712', int64), &
        int(z'9ced737bb6c4183d', int64), &
        int(z'55464dd69685606b', int64), &
        int(z'c428d05aa4751e4c', int64), &
        int(z'aa97e14c3c26b886', int64), &
        int(z'f53304714d9265df', int64), &
        int(z'd53dd99f4b3066a8', int64), &
        int(z'993fe2c6d07b7fab', int64), &
        int(z'e546a8038efe4029', int64), &
        int(z'bf8fdb78849a5f96', int64), &
        int(z'de98520472bdd033', int64), &
        int(z'ef73d256a5c0f77c', int64), &
        int(z'963e66858f6d4440', int64), &
        int(z'95a8637627989aad', int64), &
        int(z'dde7001379a44aa8', int64), &
        int(z'bb127c53b17ec159', int64), &
        int(z'5560c018580d5d52', int64), &
        int(z'e9d71b689dde71af', int64), &
        int(z'aab8f01e6e10b4a6', int64), &
        int(z'9226712162ab070d', int64), &
        int(z'cab3961304ca70e8', int64), &
        int(z'b6b00d69bb55c8d1', int64), &
        int(z'3d607b97c5fd0d22', int64), &
        int(z'e45c10c42a2b3b05', int64), &
        int(z'8cb89a7db77c506a', int64), &
        int(z'8eb98a7a9a5b04e3', int64), &
        int(z'77f3608e92adb242', int64) ]
    integer(int64), parameter :: P5_4(200) = [ &
        int(z'b267ed1940f1c61c', int64), &
        int(z'55f038b237591ed3', int64), &
        int(z'df01e85f912e37a3', int64), &
        int(z'6b6c46dec52f6688', int64), &
        int(z'8b61313bbabce2c6', int64), &
        int(z'2323ac4b3b3da015', int64), &
        int(z'ae397d8aa96c1b77', int64), &
        int(z'abec975e0a0d081a', int64), &
        int(z'd9c7dced53c72255', int64), &
        int(z'96e7bd358c904a21', int64), &
        int(z'881cea14545c7575', int64), &
        int(z'7e50d64177da2e54', int64), &
        int(z'aa242499697392d2', int64), &
        int(z'dde50bd1d5d0b9e9', int64), &
        int(z'd4ad2dbfc3d07787', int64), &
        int(z'955e4ec64b44e864', int64), &
        int(z'84ec3c97da624ab4', int64), &
        int(z'bd5af13bef0b113e', int64), &
        int(z'a6274bbdd0fadd61', int64), &
        int(z'ecb1ad8aeacdd58e', int64), &
        int(z'cfb11ead453994ba', int64), &
        int(z'67de18eda5814af2', int64), &
        int(z'81ceb32c4b43fcf4', int64), &
        int(z'80eacf948770ced7', int64), &
        int(z'a2425ff75e14fc31', int64), &
        int(z'a1258379a94d028d', int64), &
        int(z'cad2f7f5359a3b3e', int64), &
        int(z'96ee45813a04330', int64), &
        int(z'fd87b5f28300ca0d', int64), &
        int(z'8bca9d6e188853fc', int64), &
        int(z'9e74d1b791e07e48', int64), &
        int(z'775ea264cf55347e', int64), &
        int(z'c612062576589dda', int64), &
        int(z'95364afe032a819e', int64), &
        int(z'f79687aed3eec551', int64), &
        int(z'3a83ddbd83f52205', int64), &
        int(z'9abe14cd44753b52', int64), &
        int(z'c4926a9672793543', int64), &
        int(z'c16d9a0095928a27', int64), &
        int(z'75b7053c0f178294', int64), &
        int(z'f1c90080baf72cb1', int64), &
        int(z'5324c68b12dd6339', int64), &
        int(z'971da05074da7bee', int64), &
        int(z'd3f6fc16ebca5e04', int64), &
        int(z'bce5086492111aea', int64), &
        int(z'88f4bb1ca6bcf585', int64), &
        int(z'ec1e4a7db69561a5', int64), &
        int(z'2b31e9e3d06c32e6', int64), &
        int(z'9392ee8e921d5d07', int64), &
        int(z'3aff322e62439fd0', int64), &
        int(z'b877aa3236a4b449', int64), &
        int(z'9befeb9fad487c3', int64), &
        int(z'e69594bec44de15b', int64), &
        int(z'4c2ebe687989a9b4', int64), &
        int(z'901d7cf73ab0acd9', int64), &
        int(z'f9d37014bf60a11', int64), &
        int(z'b424dc35095cd80f', int64), &
        int(z'538484c19ef38c95', int64), &
        int(z'e12e13424bb40e13', int64), &
        int(z'2865a5f206b06fba', int64), &
        int(z'8cbccc096f5088cb', int64), &
        int(z'f93f87b7442e45d4', int64), &
        int(z'afebff0bcb24aafe', int64), &
        int(z'f78f69a51539d749', int64), &
        int(z'dbe6fecebdedd5be', int64), &
        int(z'b573440e5a884d1c', int64), &
        int(z'89705f4136b4a597', int64), &
        int(z'31680a88f8953031', int64), &
        int(z'abcc77118461cefc', int64), &
        int(z'fdc20d2b36ba7c3e', int64), &
        int(z'd6bf94d5e57a42bc', int64), &
        int(z'3d32907604691b4d', int64), &
        int(z'8637bd05af6c69b5', int64), &
        int(z'a63f9a49c2c1b110', int64), &
        int(z'a7c5ac471b478423', int64), &
        int(z'fcf80dc33721d54', int64), &
        int(z'd1b71758e219652b', int64), &
        int(z'd3c36113404ea4a9', int64), &
        int(z'83126e978d4fdf3b', int64), &
        int(z'645a1cac083126ea', int64), &
        int(z'a3d70a3d70a3d70a', int64), &
        int(z'3d70a3d70a3d70a4', int64), &
        int(z'cccccccccccccccc', int64), &
        int(z'cccccccccccccccd', int64), &
        int(z'8000000000000000', int64), &
        int(z'0', int64), &
        int(z'a000000000000000', int64), &
        int(z'0', int64), &
        int(z'c800000000000000', int64), &
        int(z'0', int64), &
        int(z'fa00000000000000', int64), &
        int(z'0', int64), &
        int(z'9c40000000000000', int64), &
        int(z'0', int64), &
        int(z'c350000000000000', int64), &
        int(z'0', int64), &
        int(z'f424000000000000', int64), &
        int(z'0', int64), &
        int(z'9896800000000000', int64), &
        int(z'0', int64), &
        int(z'bebc200000000000', int64), &
        int(z'0', int64), &
        int(z'ee6b280000000000', int64), &
        int(z'0', int64), &
        int(z'9502f90000000000', int64), &
        int(z'0', int64), &
        int(z'ba43b74000000000', int64), &
        int(z'0', int64), &
        int(z'e8d4a51000000000', int64), &
        int(z'0', int64), &
        int(z'9184e72a00000000', int64), &
        int(z'0', int64), &
        int(z'b5e620f480000000', int64), &
        int(z'0', int64), &
        int(z'e35fa931a0000000', int64), &
        int(z'0', int64), &
        int(z'8e1bc9bf04000000', int64), &
        int(z'0', int64), &
        int(z'b1a2bc2ec5000000', int64), &
        int(z'0', int64), &
        int(z'de0b6b3a76400000', int64), &
        int(z'0', int64), &
        int(z'8ac7230489e80000', int64), &
        int(z'0', int64), &
        int(z'ad78ebc5ac620000', int64), &
        int(z'0', int64), &
        int(z'd8d726b7177a8000', int64), &
        int(z'0', int64), &
        int(z'878678326eac9000', int64), &
        int(z'0', int64), &
        int(z'a968163f0a57b400', int64), &
        int(z'0', int64), &
        int(z'd3c21bcecceda100', int64), &
        int(z'0', int64), &
        int(z'84595161401484a0', int64), &
        int(z'0', int64), &
        int(z'a56fa5b99019a5c8', int64), &
        int(z'0', int64), &
        int(z'cecb8f27f4200f3a', int64), &
        int(z'0', int64), &
        int(z'813f3978f8940984', int64), &
        int(z'4000000000000000', int64), &
        int(z'a18f07d736b90be5', int64), &
        int(z'5000000000000000', int64), &
        int(z'c9f2c9cd04674ede', int64), &
        int(z'a400000000000000', int64), &
        int(z'fc6f7c4045812296', int64), &
        int(z'4d00000000000000', int64), &
        int(z'9dc5ada82b70b59d', int64), &
        int(z'f020000000000000', int64), &
        int(z'c5371912364ce305', int64), &
        int(z'6c28000000000000', int64), &
        int(z'f684df56c3e01bc6', int64), &
        int(z'c732000000000000', int64), &
        int(z'9a130b963a6c115c', int64), &
        int(z'3c7f400000000000', int64), &
        int(z'c097ce7bc90715b3', int64), &
        int(z'4b9f100000000000', int64), &
        int(z'f0bdc21abb48db20', int64), &
        int(z'1e86d40000000000', int64), &
        int(z'96769950b50d88f4', int64), &
        int(z'1314448000000000', int64), &
        int(z'bc143fa4e250eb31', int64), &
        int(z'17d955a000000000', int64), &
        int(z'eb194f8e1ae525fd', int64), &
        int(z'5dcfab0800000000', int64), &
        int(z'92efd1b8d0cf37be', int64), &
        int(z'5aa1cae500000000', int64), &
        int(z'b7abc627050305ad', int64), &
        int(z'f14a3d9e40000000', int64), &
        int(z'e596b7b0c643c719', int64), &
        int(z'6d9ccd05d0000000', int64), &
        int(z'8f7e32ce7bea5c6f', int64), &
        int(z'e4820023a2000000', int64), &
        int(z'b35dbf821ae4f38b', int64), &
        int(z'dda2802c8a800000', int64), &
        int(z'e0352f62a19e306e', int64), &
        int(z'd50b2037ad200000', int64), &
        int(z'8c213d9da502de45', int64), &
        int(z'4526f422cc340000', int64), &
        int(z'af298d050e4395d6', int64), &
        int(z'9670b12b7f410000', int64), &
        int(z'daf3f04651d47b4c', int64), &
        int(z'3c0cdd765f114000', int64), &
        int(z'88d8762bf324cd0f', int64), &
        int(z'a5880a69fb6ac800', int64), &
        int(z'ab0e93b6efee0053', int64), &
        int(z'8eea0d047a457a00', int64), &
        int(z'd5d238a4abe98068', int64), &
        int(z'72a4904598d6d880', int64), &
        int(z'85a36366eb71f041', int64), &
        int(z'47a6da2b7f864750', int64), &
        int(z'a70c3c40a64e6c51', int64), &
        int(z'999090b65f67d924', int64), &
        int(z'd0cf4b50cfe20765', int64), &
        int(z'fff4b4e3f741cf6d', int64), &
        int(z'82818f1281ed449f', int64), &
        int(z'bff8f10e7a8921a4', int64), &
        int(z'a321f2d7226895c7', int64), &
        int(z'aff72d52192b6a0d', int64) ]
    integer(int64), parameter :: P5_5(200) = [ &
        int(z'cbea6f8ceb02bb39', int64), &
        int(z'9bf4f8a69f764490', int64), &
        int(z'fee50b7025c36a08', int64), &
        int(z'2f236d04753d5b4', int64), &
        int(z'9f4f2726179a2245', int64), &
        int(z'1d762422c946590', int64), &
        int(z'c722f0ef9d80aad6', int64), &
        int(z'424d3ad2b7b97ef5', int64), &
        int(z'f8ebad2b84e0d58b', int64), &
        int(z'd2e0898765a7deb2', int64), &
        int(z'9b934c3b330c8577', int64), &
        int(z'63cc55f49f88eb2f', int64), &
        int(z'c2781f49ffcfa6d5', int64), &
        int(z'3cbf6b71c76b25fb', int64), &
        int(z'f316271c7fc3908a', int64), &
        int(z'8bef464e3945ef7a', int64), &
        int(z'97edd871cfda3a56', int64), &
        int(z'97758bf0e3cbb5ac', int64), &
        int(z'bde94e8e43d0c8ec', int64), &
        int(z'3d52eeed1cbea317', int64), &
        int(z'ed63a231d4c4fb27', int64), &
        int(z'4ca7aaa863ee4bdd', int64), &
        int(z'945e455f24fb1cf8', int64), &
        int(z'8fe8caa93e74ef6a', int64), &
        int(z'b975d6b6ee39e436', int64), &
        int(z'b3e2fd538e122b44', int64), &
        int(z'e7d34c64a9c85d44', int64), &
        int(z'60dbbca87196b616', int64), &
        int(z'90e40fbeea1d3a4a', int64), &
        int(z'bc8955e946fe31cd', int64), &
        int(z'b51d13aea4a488dd', int64), &
        int(z'6babab6398bdbe41', int64), &
        int(z'e264589a4dcdab14', int64), &
        int(z'c696963c7eed2dd1', int64), &
        int(z'8d7eb76070a08aec', int64), &
        int(z'fc1e1de5cf543ca2', int64), &
        int(z'b0de65388cc8ada8', int64), &
        int(z'3b25a55f43294bcb', int64), &
        int(z'dd15fe86affad912', int64), &
        int(z'49ef0eb713f39ebe', int64), &
        int(z'8a2dbf142dfcc7ab', int64), &
        int(z'6e3569326c784337', int64), &
        int(z'acb92ed9397bf996', int64), &
        int(z'49c2c37f07965404', int64), &
        int(z'd7e77a8f87daf7fb', int64), &
        int(z'dc33745ec97be906', int64), &
        int(z'86f0ac99b4e8dafd', int64), &
        int(z'69a028bb3ded71a3', int64), &
        int(z'a8acd7c0222311bc', int64), &
        int(z'c40832ea0d68ce0c', int64), &
        int(z'd2d80db02aabd62b', int64), &
        int(z'f50a3fa490c30190', int64), &
        int(z'83c7088e1aab65db', int64), &
        int(z'792667c6da79e0fa', int64), &
        int(z'a4b8cab1a1563f52', int64), &
        int(z'577001b891185938', int64), &
        int(z'cde6fd5e09abcf26', int64), &
        int(z'ed4c0226b55e6f86', int64), &
        int(z'80b05e5ac60b6178', int64), &
        int(z'544f8158315b05b4', int64), &
        int(z'a0dc75f1778e39d6', int64), &
        int(z'696361ae3db1c721', int64), &
        int(z'c913936dd571c84c', int64), &
        int(z'3bc3a19cd1e38e9', int64), &
        int(z'fb5878494ace3a5f', int64), &
        int(z'4ab48a04065c723', int64), &
        int(z'9d174b2dcec0e47b', int64), &
        int(z'62eb0d64283f9c76', int64), &
        int(z'c45d1df942711d9a', int64), &
        int(z'3ba5d0bd324f8394', int64), &
        int(z'f5746577930d6500', int64), &
        int(z'ca8f44ec7ee36479', int64), &
        int(z'9968bf6abbe85f20', int64), &
        int(z'7e998b13cf4e1ecb', int64), &
        int(z'bfc2ef456ae276e8', int64), &
        int(z'9e3fedd8c321a67e', int64), &
        int(z'efb3ab16c59b14a2', int64), &
        int(z'c5cfe94ef3ea101e', int64), &
        int(z'95d04aee3b80ece5', int64), &
        int(z'bba1f1d158724a12', int64), &
        int(z'bb445da9ca61281f', int64), &
        int(z'2a8a6e45ae8edc97', int64), &
        int(z'ea1575143cf97226', int64), &
        int(z'f52d09d71a3293bd', int64), &
        int(z'924d692ca61be758', int64), &
        int(z'593c2626705f9c56', int64), &
        int(z'b6e0c377cfa2e12e', int64), &
        int(z'6f8b2fb00c77836c', int64), &
        int(z'e498f455c38b997a', int64), &
        int(z'b6dfb9c0f956447', int64), &
        int(z'8edf98b59a373fec', int64), &
        int(z'4724bd4189bd5eac', int64), &
        int(z'b2977ee300c50fe7', int64), &
        int(z'58edec91ec2cb657', int64), &
        int(z'df3d5e9bc0f653e1', int64), &
        int(z'2f2967b66737e3ed', int64), &
        int(z'8b865b215899f46c', int64), &
        int(z'bd79e0d20082ee74', int64), &
        int(z'ae67f1e9aec07187', int64), &
        int(z'ecd8590680a3aa11', int64), &
        int(z'da01ee641a708de9', int64), &
        int(z'e80e6f4820cc9495', int64), &
        int(z'884134fe908658b2', int64), &
        int(z'3109058d147fdcdd', int64), &
        int(z'aa51823e34a7eede', int64), &
        int(z'bd4b46f0599fd415', int64), &
        int(z'd4e5e2cdc1d1ea96', int64), &
        int(z'6c9e18ac7007c91a', int64), &
        int(z'850fadc09923329e', int64), &
        int(z'3e2cf6bc604ddb0', int64), &
        int(z'a6539930bf6bff45', int64), &
        int(z'84db8346b786151c', int64), &
        int(z'cfe87f7cef46ff16', int64), &
        int(z'e612641865679a63', int64), &
        int(z'81f14fae158c5f6e', int64), &
        int(z'4fcb7e8f3f60c07e', int64), &
        int(z'a26da3999aef7749', int64), &
        int(z'e3be5e330f38f09d', int64), &
        int(z'cb090c8001ab551c', int64), &
        int(z'5cadf5bfd3072cc5', int64), &
        int(z'fdcb4fa002162a63', int64), &
        int(z'73d9732fc7c8f7f6', int64), &
        int(z'9e9f11c4014dda7e', int64), &
        int(z'2867e7fddcdd9afa', int64), &
        int(z'c646d63501a1511d', int64), &
        int(z'b281e1fd541501b8', int64), &
        int(z'f7d88bc24209a565', int64), &
        int(z'1f225a7ca91a4226', int64), &
        int(z'9ae757596946075f', int64), &
        int(z'3375788de9b06958', int64), &
        int(z'c1a12d2fc3978937', int64), &
        int(z'52d6b1641c83ae', int64), &
        int(z'f209787bb47d6b84', int64), &
        int(z'c0678c5dbd23a49a', int64), &
        int(z'9745eb4d50ce6332', int64), &
        int(z'f840b7ba963646e0', int64), &
        int(z'bd176620a501fbff', int64), &
        int(z'b650e5a93bc3d898', int64), &
        int(z'ec5d3fa8ce427aff', int64), &
        int(z'a3e51f138ab4cebe', int64), &
        int(z'93ba47c980e98cdf', int64), &
        int(z'c66f336c36b10137', int64), &
        int(z'b8a8d9bbe123f017', int64), &
        int(z'b80b0047445d4184', int64), &
        int(z'e6d3102ad96cec1d', int64), &
        int(z'a60dc059157491e5', int64), &
        int(z'9043ea1ac7e41392', int64), &
        int(z'87c89837ad68db2f', int64), &
        int(z'b454e4a179dd1877', int64), &
        int(z'29babe4598c311fb', int64), &
        int(z'e16a1dc9d8545e94', int64), &
        int(z'f4296dd6fef3d67a', int64), &
        int(z'8ce2529e2734bb1d', int64), &
        int(z'1899e4a65f58660c', int64), &
        int(z'b01ae745b101e9e4', int64), &
        int(z'5ec05dcff72e7f8f', int64), &
        int(z'dc21a1171d42645d', int64), &
        int(z'76707543f4fa1f73', int64), &
        int(z'899504ae72497eba', int64), &
        int(z'6a06494a791c53a8', int64), &
        int(z'abfa45da0edbde69', int64), &
        int(z'487db9d17636892', int64), &
        int(z'd6f8d7509292d603', int64), &
        int(z'45a9d2845d3c42b6', int64), &
        int(z'865b86925b9bc5c2', int64), &
        int(z'b8a2392ba45a9b2', int64), &
        int(z'a7f26836f282b732', int64), &
        int(z'8e6cac7768d7141e', int64), &
        int(z'd1ef0244af2364ff', int64), &
        int(z'3207d795430cd926', int64), &
        int(z'8335616aed761f1f', int64), &
        int(z'7f44e6bd49e807b8', int64), &
        int(z'a402b9c5a8d3a6e7', int64), &
        int(z'5f16206c9c6209a6', int64), &
        int(z'cd036837130890a1', int64), &
        int(z'36dba887c37a8c0f', int64), &
        int(z'802221226be55a64', int64), &
        int(z'c2494954da2c9789', int64), &
        int(z'a02aa96b06deb0fd', int64), &
        int(z'f2db9baa10b7bd6c', int64), &
        int(z'c83553c5c8965d3d', int64), &
        int(z'6f92829494e5acc7', int64), &
        int(z'fa42a8b73abbf48c', int64), &
        int(z'cb772339ba1f17f9', int64), &
        int(z'9c69a97284b578d7', int64), &
        int(z'ff2a760414536efb', int64), &
        int(z'c38413cf25e2d70d', int64), &
        int(z'fef5138519684aba', int64), &
        int(z'f46518c2ef5b8cd1', int64), &
        int(z'7eb258665fc25d69', int64), &
        int(z'98bf2f79d5993802', int64), &
        int(z'ef2f773ffbd97a61', int64), &
        int(z'beeefb584aff8603', int64), &
        int(z'aafb550ffacfd8fa', int64), &
        int(z'eeaaba2e5dbf6784', int64), &
        int(z'95ba2a53f983cf38', int64), &
        int(z'952ab45cfa97a0b2', int64), &
        int(z'dd945a747bf26183', int64), &
        int(z'ba756174393d88df', int64), &
        int(z'94f971119aeef9e4', int64) ]
    integer(int64), parameter :: P5_6(200) = [ &
        int(z'e912b9d1478ceb17', int64), &
        int(z'7a37cd5601aab85d', int64), &
        int(z'91abb422ccb812ee', int64), &
        int(z'ac62e055c10ab33a', int64), &
        int(z'b616a12b7fe617aa', int64), &
        int(z'577b986b314d6009', int64), &
        int(z'e39c49765fdf9d94', int64), &
        int(z'ed5a7e85fda0b80b', int64), &
        int(z'8e41ade9fbebc27d', int64), &
        int(z'14588f13be847307', int64), &
        int(z'b1d219647ae6b31c', int64), &
        int(z'596eb2d8ae258fc8', int64), &
        int(z'de469fbd99a05fe3', int64), &
        int(z'6fca5f8ed9aef3bb', int64), &
        int(z'8aec23d680043bee', int64), &
        int(z'25de7bb9480d5854', int64), &
        int(z'ada72ccc20054ae9', int64), &
        int(z'af561aa79a10ae6a', int64), &
        int(z'd910f7ff28069da4', int64), &
        int(z'1b2ba1518094da04', int64), &
        int(z'87aa9aff79042286', int64), &
        int(z'90fb44d2f05d0842', int64), &
        int(z'a99541bf57452b28', int64), &
        int(z'353a1607ac744a53', int64), &
        int(z'd3fa922f2d1675f2', int64), &
        int(z'42889b8997915ce8', int64), &
        int(z'847c9b5d7c2e09b7', int64), &
        int(z'69956135febada11', int64), &
        int(z'a59bc234db398c25', int64), &
        int(z'43fab9837e699095', int64), &
        int(z'cf02b2c21207ef2e', int64), &
        int(z'94f967e45e03f4bb', int64), &
        int(z'8161afb94b44f57d', int64), &
        int(z'1d1be0eebac278f5', int64), &
        int(z'a1ba1ba79e1632dc', int64), &
        int(z'6462d92a69731732', int64), &
        int(z'ca28a291859bbf93', int64), &
        int(z'7d7b8f7503cfdcfe', int64), &
        int(z'fcb2cb35e702af78', int64), &
        int(z'5cda735244c3d43e', int64), &
        int(z'9defbf01b061adab', int64), &
        int(z'3a0888136afa64a7', int64), &
        int(z'c56baec21c7a1916', int64), &
        int(z'88aaa1845b8fdd0', int64), &
        int(z'f6c69a72a3989f5b', int64), &
        int(z'8aad549e57273d45', int64), &
        int(z'9a3c2087a63f6399', int64), &
        int(z'36ac54e2f678864b', int64), &
        int(z'c0cb28a98fcf3c7f', int64), &
        int(z'84576a1bb416a7dd', int64), &
        int(z'f0fdf2d3f3c30b9f', int64), &
        int(z'656d44a2a11c51d5', int64), &
        int(z'969eb7c47859e743', int64), &
        int(z'9f644ae5a4b1b325', int64), &
        int(z'bc4665b596706114', int64), &
        int(z'873d5d9f0dde1fee', int64), &
        int(z'eb57ff22fc0c7959', int64), &
        int(z'a90cb506d155a7ea', int64), &
        int(z'9316ff75dd87cbd8', int64), &
        int(z'9a7f12442d588f2', int64), &
        int(z'b7dcbf5354e9bece', int64), &
        int(z'c11ed6d538aeb2f', int64), &
        int(z'e5d3ef282a242e81', int64), &
        int(z'8f1668c8a86da5fa', int64), &
        int(z'8fa475791a569d10', int64), &
        int(z'f96e017d694487bc', int64), &
        int(z'b38d92d760ec4455', int64), &
        int(z'37c981dcc395a9ac', int64), &
        int(z'e070f78d3927556a', int64), &
        int(z'85bbe253f47b1417', int64), &
        int(z'8c469ab843b89562', int64), &
        int(z'93956d7478ccec8e', int64), &
        int(z'af58416654a6babb', int64), &
        int(z'387ac8d1970027b2', int64), &
        int(z'db2e51bfe9d0696a', int64), &
        int(z'6997b05fcc0319e', int64), &
        int(z'88fcf317f22241e2', int64), &
        int(z'441fece3bdf81f03', int64), &
        int(z'ab3c2fddeeaad25a', int64), &
        int(z'd527e81cad7626c3', int64), &
        int(z'd60b3bd56a5586f1', int64), &
        int(z'8a71e223d8d3b074', int64), &
        int(z'85c7056562757456', int64), &
        int(z'f6872d5667844e49', int64), &
        int(z'a738c6bebb12d16c', int64), &
        int(z'b428f8ac016561db', int64), &
        int(z'd106f86e69d785c7', int64), &
        int(z'e13336d701beba52', int64), &
        int(z'82a45b450226b39c', int64), &
        int(z'ecc0024661173473', int64), &
        int(z'a34d721642b06084', int64), &
        int(z'27f002d7f95d0190', int64), &
        int(z'cc20ce9bd35c78a5', int64), &
        int(z'31ec038df7b441f4', int64), &
        int(z'ff290242c83396ce', int64), &
        int(z'7e67047175a15271', int64), &
        int(z'9f79a169bd203e41', int64), &
        int(z'f0062c6e984d386', int64), &
        int(z'c75809c42c684dd1', int64), &
        int(z'52c07b78a3e60868', int64), &
        int(z'f92e0c3537826145', int64), &
        int(z'a7709a56ccdf8a82', int64), &
        int(z'9bbcc7a142b17ccb', int64), &
        int(z'88a66076400bb691', int64), &
        int(z'c2abf989935ddbfe', int64), &
        int(z'6acff893d00ea435', int64), &
        int(z'f356f7ebf83552fe', int64), &
        int(z'583f6b8c4124d43', int64), &
        int(z'98165af37b2153de', int64), &
        int(z'c3727a337a8b704a', int64), &
        int(z'be1bf1b059e9a8d6', int64), &
        int(z'744f18c0592e4c5c', int64), &
        int(z'eda2ee1c7064130c', int64), &
        int(z'1162def06f79df73', int64), &
        int(z'9485d4d1c63e8be7', int64), &
        int(z'8addcb5645ac2ba8', int64), &
        int(z'b9a74a0637ce2ee1', int64), &
        int(z'6d953e2bd7173692', int64), &
        int(z'e8111c87c5c1ba99', int64), &
        int(z'c8fa8db6ccdd0437', int64), &
        int(z'910ab1d4db9914a0', int64), &
        int(z'1d9c9892400a22a2', int64), &
        int(z'b54d5e4a127f59c8', int64), &
        int(z'2503beb6d00cab4b', int64), &
        int(z'e2a0b5dc971f303a', int64), &
        int(z'2e44ae64840fd61d', int64), &
        int(z'8da471a9de737e24', int64), &
        int(z'5ceaecfed289e5d2', int64), &
        int(z'b10d8e1456105dad', int64), &
        int(z'7425a83e872c5f47', int64), &
        int(z'dd50f1996b947518', int64), &
        int(z'd12f124e28f77719', int64), &
        int(z'8a5296ffe33cc92f', int64), &
        int(z'82bd6b70d99aaa6f', int64), &
        int(z'ace73cbfdc0bfb7b', int64), &
        int(z'636cc64d1001550b', int64), &
        int(z'd8210befd30efa5a', int64), &
        int(z'3c47f7e05401aa4e', int64), &
        int(z'8714a775e3e95c78', int64), &
        int(z'65acfaec34810a71', int64), &
        int(z'a8d9d1535ce3b396', int64), &
        int(z'7f1839a741a14d0d', int64), &
        int(z'd31045a8341ca07c', int64), &
        int(z'1ede48111209a050', int64), &
        int(z'83ea2b892091e44d', int64), &
        int(z'934aed0aab460432', int64), &
        int(z'a4e4b66b68b65d60', int64), &
        int(z'f81da84d5617853f', int64), &
        int(z'ce1de40642e3f4b9', int64), &
        int(z'36251260ab9d668e', int64), &
        int(z'80d2ae83e9ce78f3', int64), &
        int(z'c1d72b7c6b426019', int64), &
        int(z'a1075a24e4421730', int64), &
        int(z'b24cf65b8612f81f', int64), &
        int(z'c94930ae1d529cfc', int64), &
        int(z'dee033f26797b627', int64), &
        int(z'fb9b7cd9a4a7443c', int64), &
        int(z'169840ef017da3b1', int64), &
        int(z'9d412e0806e88aa5', int64), &
        int(z'8e1f289560ee864e', int64), &
        int(z'c491798a08a2ad4e', int64), &
        int(z'f1a6f2bab92a27e2', int64), &
        int(z'f5b5d7ec8acb58a2', int64), &
        int(z'ae10af696774b1db', int64), &
        int(z'9991a6f3d6bf1765', int64), &
        int(z'acca6da1e0a8ef29', int64), &
        int(z'bff610b0cc6edd3f', int64), &
        int(z'17fd090a58d32af3', int64), &
        int(z'eff394dcff8a948e', int64), &
        int(z'ddfc4b4cef07f5b0', int64), &
        int(z'95f83d0a1fb69cd9', int64), &
        int(z'4abdaf101564f98e', int64), &
        int(z'bb764c4ca7a4440f', int64), &
        int(z'9d6d1ad41abe37f1', int64), &
        int(z'ea53df5fd18d5513', int64), &
        int(z'84c86189216dc5ed', int64), &
        int(z'92746b9be2f8552c', int64), &
        int(z'32fd3cf5b4e49bb4', int64), &
        int(z'b7118682dbb66a77', int64), &
        int(z'3fbc8c33221dc2a1', int64), &
        int(z'e4d5e82392a40515', int64), &
        int(z'fabaf3feaa5334a', int64), &
        int(z'8f05b1163ba6832d', int64), &
        int(z'29cb4d87f2a7400e', int64), &
        int(z'b2c71d5bca9023f8', int64), &
        int(z'743e20e9ef511012', int64), &
        int(z'df78e4b2bd342cf6', int64), &
        int(z'914da9246b255416', int64), &
        int(z'8bab8eefb6409c1a', int64), &
        int(z'1ad089b6c2f7548e', int64), &
        int(z'ae9672aba3d0c320', int64), &
        int(z'a184ac2473b529b1', int64), &
        int(z'da3c0f568cc4f3e8', int64), &
        int(z'c9e5d72d90a2741e', int64), &
        int(z'8865899617fb1871', int64), &
        int(z'7e2fa67c7a658892', int64), &
        int(z'aa7eebfb9df9de8d', int64), &
        int(z'ddbb901b98feeab7', int64), &
        int(z'd51ea6fa85785631', int64), &
        int(z'552a74227f3ea565', int64) ]
    integer(int64), parameter :: P5_7(102) = [ &
        int(z'8533285c936b35de', int64), &
        int(z'd53a88958f87275f', int64), &
        int(z'a67ff273b8460356', int64), &
        int(z'8a892abaf368f137', int64), &
        int(z'd01fef10a657842c', int64), &
        int(z'2d2b7569b0432d85', int64), &
        int(z'8213f56a67f6b29b', int64), &
        int(z'9c3b29620e29fc73', int64), &
        int(z'a298f2c501f45f42', int64), &
        int(z'8349f3ba91b47b8f', int64), &
        int(z'cb3f2f7642717713', int64), &
        int(z'241c70a936219a73', int64), &
        int(z'fe0efb53d30dd4d7', int64), &
        int(z'ed238cd383aa0110', int64), &
        int(z'9ec95d1463e8a506', int64), &
        int(z'f4363804324a40aa', int64), &
        int(z'c67bb4597ce2ce48', int64), &
        int(z'b143c6053edcd0d5', int64), &
        int(z'f81aa16fdc1b81da', int64), &
        int(z'dd94b7868e94050a', int64), &
        int(z'9b10a4e5e9913128', int64), &
        int(z'ca7cf2b4191c8326', int64), &
        int(z'c1d4ce1f63f57d72', int64), &
        int(z'fd1c2f611f63a3f0', int64), &
        int(z'f24a01a73cf2dccf', int64), &
        int(z'bc633b39673c8cec', int64), &
        int(z'976e41088617ca01', int64), &
        int(z'd5be0503e085d813', int64), &
        int(z'bd49d14aa79dbc82', int64), &
        int(z'4b2d8644d8a74e18', int64), &
        int(z'ec9c459d51852ba2', int64), &
        int(z'ddf8e7d60ed1219e', int64), &
        int(z'93e1ab8252f33b45', int64), &
        int(z'cabb90e5c942b503', int64), &
        int(z'b8da1662e7b00a17', int64), &
        int(z'3d6a751f3b936243', int64), &
        int(z'e7109bfba19c0c9d', int64), &
        int(z'cc512670a783ad4', int64), &
        int(z'906a617d450187e2', int64), &
        int(z'27fb2b80668b24c5', int64), &
        int(z'b484f9dc9641e9da', int64), &
        int(z'b1f9f660802dedf6', int64), &
        int(z'e1a63853bbd26451', int64), &
        int(z'5e7873f8a0396973', int64), &
        int(z'8d07e33455637eb2', int64), &
        int(z'db0b487b6423e1e8', int64), &
        int(z'b049dc016abc5e5f', int64), &
        int(z'91ce1a9a3d2cda62', int64), &
        int(z'dc5c5301c56b75f7', int64), &
        int(z'7641a140cc7810fb', int64), &
        int(z'89b9b3e11b6329ba', int64), &
        int(z'a9e904c87fcb0a9d', int64), &
        int(z'ac2820d9623bf429', int64), &
        int(z'546345fa9fbdcd44', int64), &
        int(z'd732290fbacaf133', int64), &
        int(z'a97c177947ad4095', int64), &
        int(z'867f59a9d4bed6c0', int64), &
        int(z'49ed8eabcccc485d', int64), &
        int(z'a81f301449ee8c70', int64), &
        int(z'5c68f256bfff5a74', int64), &
        int(z'd226fc195c6a2f8c', int64), &
        int(z'73832eec6fff3111', int64), &
        int(z'83585d8fd9c25db7', int64), &
        int(z'c831fd53c5ff7eab', int64), &
        int(z'a42e74f3d032f525', int64), &
        int(z'ba3e7ca8b77f5e55', int64), &
        int(z'cd3a1230c43fb26f', int64), &
        int(z'28ce1bd2e55f35eb', int64), &
        int(z'80444b5e7aa7cf85', int64), &
        int(z'7980d163cf5b81b3', int64), &
        int(z'a0555e361951c366', int64), &
        int(z'd7e105bcc332621f', int64), &
        int(z'c86ab5c39fa63440', int64), &
        int(z'8dd9472bf3fefaa7', int64), &
        int(z'fa856334878fc150', int64), &
        int(z'b14f98f6f0feb951', int64), &
        int(z'9c935e00d4b9d8d2', int64), &
        int(z'6ed1bf9a569f33d3', int64), &
        int(z'c3b8358109e84f07', int64), &
        int(z'a862f80ec4700c8', int64), &
        int(z'f4a642e14c6262c8', int64), &
        int(z'cd27bb612758c0fa', int64), &
        int(z'98e7e9cccfbd7dbd', int64), &
        int(z'8038d51cb897789c', int64), &
        int(z'bf21e44003acdd2c', int64), &
        int(z'e0470a63e6bd56c3', int64), &
        int(z'eeea5d5004981478', int64), &
        int(z'1858ccfce06cac74', int64), &
        int(z'95527a5202df0ccb', int64), &
        int(z'f37801e0c43ebc8', int64), &
        int(z'baa718e68396cffd', int64), &
        int(z'd30560258f54e6ba', int64), &
        int(z'e950df20247c83fd', int64), &
        int(z'47c6b82ef32a2069', int64), &
        int(z'91d28b7416cdd27e', int64), &
        int(z'4cdc331d57fa5441', int64), &
        int(z'b6472e511c81471d', int64), &
        int(z'e0133fe4adf8e952', int64), &
        int(z'e3d8f9e563a198e5', int64), &
        int(z'58180fddd97723a6', int64), &
        int(z'8e679c2f5e44ff8f', int64), &
        int(z'570f09eaa7ea7648', int64) ]
    integer(int64), parameter :: P5(P5CNT) = &
        [ P5_1, P5_2, P5_3, P5_4, P5_5, P5_6, P5_7 ]

    real(real64),   parameter ::  DPOW10(-22:22) = [ (10**real(i,real64), i=-22, 22) ]
    real(real32),   parameter ::  FPOW10(-10:10) = [ (10**real(i,real32), i=-10, 10) ]

    integer(int64), parameter :: DMAXM(0:23) = [ &
        9007199254740992_int64, &
        1801439850948198_int64, &
        360287970189639_int64, &
        72057594037927_int64, &
        14411518807585_int64, &
        2882303761517_int64, &
        576460752303_int64, &
        115292150460_int64, &
        23058430092_int64, &
        4611686018_int64, &
        922337203_int64, &
        184467440_int64, &
        36893488_int64, &
        7378697_int64, &
        1475739_int64, &
        295147_int64, &
        59029_int64, &
        11805_int64, &
        2361_int64, &
        472_int64, &
        94_int64, &
        18_int64, &
        3_int64, &
        0_int64 ]

    integer(int64), parameter :: FMAXM(0:11) = [ &
        16777216_int64, &
        3355443_int64, &
        671088_int64, &
        134217_int64, &
        26843_int64, &
        5368_int64, &
        1073_int64, &
        214_int64, &
        42_int64, &
        8_int64, &
        1_int64, &
        0_int64 ]

    integer(int64), parameter :: P10U64(0:19) = [ [(10_int64**i,i=0,18)], int(z'8AC7230489E80000', int64) ]

    integer, parameter :: MXDG64(2:36) = [ &
        64, 41, 32, 28, 25, 23, 22, 21, 20, 19, 18, 18, 17, 17, 16, 16, 16, 16, &
        15, 15, 15, 15, 14, 14, 14, 14, 14, 14, 14, 13, 13, 13, 13, 13, 13 ]

    integer(int64), parameter :: MSAFE64(2:36) = [ &
        int(z'8000000000000000', int64), &
        int(z'a8b8b452291fe821', int64), &
        int(z'4000000000000000', int64), &
        int(z'6765c793fa10079d', int64), &
        int(z'41c21cb8e1000000', int64), &
        int(z'3642798750226111', int64), &
        int(z'8000000000000000', int64), &
        int(z'a8b8b452291fe821', int64), &
        int(z'8ac7230489e80000', int64), &
        int(z'4d28cb56c33fa539', int64), &
        int(z'1eca170c00000000', int64), &
        int(z'780c7372621bd74d', int64), &
        int(z'1e39a5057d810000', int64), &
        int(z'5b27ac993df97701', int64), &
        int(z'1000000000000000', int64), &
        int(z'27b95e997e21d9f1', int64), &
        int(z'5da0e1e53c5c8000', int64), &
        int(z'd2ae3299c1c4aedb', int64), &
        int(z'16bcc41e90000000', int64), &
        int(z'2d04b7fdd9c0ef49', int64), &
        int(z'5658597bcaa24000', int64), &
        int(z'a0e2073737609371', int64), &
        int(z'0c29e98000000000', int64), &
        int(z'14adf4b7320334b9', int64), &
        int(z'226ed36478bfa000', int64), &
        int(z'383d9170b85ff80b', int64), &
        int(z'5a3c23e39c000000', int64), &
        int(z'8e65137388122bcd', int64), &
        int(z'dd41bb36d259e000', int64), &
        int(z'0aee5720ee830681', int64), &
        int(z'1000000000000000', int64), &
        int(z'172588ad4f5f0981', int64), &
        int(z'211e44f7d02c1000', int64), &
        int(z'2ee56725f06e5c71', int64), &
        int(z'41c21cb8e1000000', int64) ]

    integer, parameter :: P5LS = 135
    integer(int64), parameter :: P5S(0:27) = [ &
        1_int64, &
        5_int64, &
        25_int64, &
        125_int64, &
        625_int64, &
        3125_int64, &
        15625_int64, &
        78125_int64, &
        390625_int64, &
        1953125_int64, &
        9765625_int64, &
        48828125_int64, &
        244140625_int64, &
        1220703125_int64, &
        6103515625_int64, &
        30517578125_int64, &
        152587890625_int64, &
        762939453125_int64, &
        3814697265625_int64, &
        19073486328125_int64, &
        95367431640625_int64, &
        476837158203125_int64, &
        2384185791015625_int64, &
        11920928955078125_int64, &
        59604644775390625_int64, &
        298023223876953125_int64, &
        1490116119384765625_int64, &
        7450580596923828125_int64 ]

    integer(int64), parameter :: P5L(5) = [ &
        int(z'13a1d71cff1b172d', int64), &
        int(z'7f682d3defa07617', int64), &
        int(z'3f0131e7ff8c90c0', int64), &
        int(z'917b01773fdcb9fe', int64), &
        int(z'02c06b9d16c407a7', int64) ]


    integer, parameter :: C2D(0:255) = [ &
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, &
          0,   1,   2,   3,   4,   5,   6,   7,   8,   9, 255, 255, 255, 255, 255, 255, &
        255,  10,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24, &
         25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35, 255, 255, 255, 255, 255, &
        255,  10,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24, &
         25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 ]

    interface fchars
        module procedure fchars_64
        module procedure fchars_32
    end interface fchars

contains

    !> Unified fast path for integers and simple fixed-point decimals.
    !> Handles: "123", "-42", "3.14", "-65.613616999999977"
    !> Bails out to pns for: scientific notation, Fortran 'd/D' format,
    !> custom decimal point, or numbers with >19 total digits.
    pure elemental subroutine try_fast(first, last, str, opts, bj, a)
        integer, intent(in) :: first, last
        character(len=*), intent(in) :: str
        type(ffc_parse_options), intent(in) :: opts
        logical, intent(in) :: bj
        type(fparsed), intent(out) :: a
        integer(int64) :: mantissa
        integer :: int_digits, frac_digits, p, ic

        !a = fparsed()
        a%valid = .false.
        if (first > last) return
        if (opts%decimal_point /= '.') return
        if (iand(opts%format, FMT_FORT) /= 0) return
        if (iand(opts%format, FMT_SKIP) /= 0) return
        if (iand(opts%format, FMT_FIX) == 0) return

        p = first
        if (str(p:p) == '-') then
            a%neg = .true.
            p = p + 1
        else if (str(p:p) == '+') then
            if (bj .or. iand(opts%format, FMT_PLUS) == 0) return
            p = p + 1
        end if
        if (p > last) return

        ! Parse integer digits
        a%ips = p
        mantissa = 0_int64
        int_digits = 0
        do while (p <= last .and. int_digits < 19)
            ic = iachar(str(p:p)) - 48
            if (ic < 0 .or. ic > 9) exit
            mantissa = 10_int64 * mantissa + int(ic, int64)
            int_digits = int_digits + 1
            p = p + 1
        end do
        if (int_digits == 0) return
        if (bj .and. int_digits > 1 .and. str(a%ips:a%ips) == '0') return
        a%ipl = int_digits

        ! Check what follows the integer part
        if (p > last) then
            ! Pure integer: consumed entire string
            a%mantissa = mantissa
            a%exponent = 0_int64
            a%lastm = p
            a%valid = .true.
            return
        end if

        ! If next char is not '.', bail out (exponent, trailing chars, etc.)
        if (str(p:p) /= '.') return
        if (iand(opts%format, FMT_SCI) == 0) return

        ! Parse fractional part
        p = p + 1
        if (p > last) return
        frac_digits = last - p + 1
        if (int_digits + frac_digits > 19) return
        a%fps = p
        call lp8(p, last, str, mantissa)
        do while (p <= last)
            ic = iachar(str(p:p)) - 48
            if (ic < 0 .or. ic > 9) return
            mantissa = 10_int64 * mantissa + int(ic, int64)
            p = p + 1
        end do
        if (frac_digits == 0) return

        a%fpl      = frac_digits
        a%mantissa = mantissa
        a%exponent = -int(frac_digits, int64)
        a%lastm = p
        a%valid = .true.
    end subroutine try_fast

    pure elemental logical function ult(a, b)
        integer(int64), intent(in) :: a, b
        ult = ieor(a, SB64) < ieor(b, SB64)
    end function ult

    pure elemental logical function uge(a, b)
        integer(int64), intent(in) :: a, b
        uge = ieor(a, SB64) >= ieor(b, SB64)
    end function uge

    pure elemental logical function ugt(a, b)
        integer(int64), intent(in) :: a, b
        ugt = ieor(a, SB64) > ieor(b, SB64)
    end function ugt

    pure elemental subroutine mu64(a, b, res)
        integer(int64), intent(in) :: a, b
        type(u128), intent(out) :: res
        if (HAS_INT128) then
            block
                integer(IK128) :: za, zb, z
                if (LITTLE_ENDIAN) then
                    za = transfer([a, 0_int64], za)
                    zb = transfer([b, 0_int64], zb)
                    z  = za * zb
                    res = transfer(z, res)
                else
                    za = transfer([0_int64, a], za)
                    zb = transfer([0_int64, b], zb)
                    z  = za * zb
                    res%lo = int(z, int64)
                    res%hi = int(ishft(z, -64), int64)
                end if
            end block
        else
            block
                integer(int64) :: a0,a1,b0,b1,w0,t,w1,w2
                a0 = iand(a, M32)
                a1 = iand(ishft(a, -32), M32)
                b0 = iand(b, M32)
                b1 = iand(ishft(b, -32), M32)
                w0 = a0 * b0
                t  = a1*b0 + iand(ishft(w0,-32), M32)
                w1 = iand(t, M32)
                w2 = iand(ishft(t,-32), M32)
                w1 = w1 + a0 * b1
                res%lo = a * b
                res%hi = a1*b1 + w2 + iand(ishft(w1,-32), M32)
            end block
        end if
    end subroutine mu64

    pure elemental integer(int64) function gdbb(d)
        real(real64), intent(in) :: d
        gdbb = transfer(d, 0_int64)
    end function gdbb

    pure elemental integer(int32) function gfbb(f)
        real(real32), intent(in) :: f
        gfbb = transfer(f, 0_int32)
    end function gfbb

    pure elemental integer function clz(x)
        integer(int64), intent(in) :: x
        clz = merge(64,leadz(x),x==0)
    end function clz

    pure elemental logical function isd(c)
        character, intent(in) :: c
        integer :: ic
        ic = iachar(c); isd = ic>=48 .and. ic<=57
    end function isd

    pure elemental logical function issp(c)
        character, intent(in) :: c
        integer :: ic
        ic = iachar(c)
        issp = (ic>=9 .and. ic<=13).or.ic==32
    end function issp

    pure elemental integer function c2dg(c)
        character, intent(in) :: c
        integer :: ic
        ! Clamp IC since 
        ic = iachar(c); c2dg = 255
        if (ic>=0 .and. ic<=255) c2dg = C2D(ic)
    end function c2dg

    ! Reinterpret 8 characters as a single int64 (little-endian byte order).
    ! p8sw/is8d expect LE layout: str(1:1) in LSB, str(8:8) in MSB.
    pure elemental integer(int64) function r8(str)
        character(len=8), intent(in) :: str
        integer :: j
        if (LITTLE_ENDIAN) then
            r8 = transfer(str, 0_int64)
        else
            r8 = iachar(str(1:1), kind=int64)
            do j = 2, 8
                r8 = ior(r8, ishft(int(iachar(str(j:j)), int64), 8*(j-1)))
            end do
        end if
    end function r8

    pure elemental logical function is8d(val)
        integer(int64), intent(in) :: val
        integer(int64) :: v1, v2
        v1 = val + int(z'4646464646464646', int64)
        v2 = val - int(z'3030303030303030', int64)
        is8d = iand(ior(v1,v2), int(z'8080808080808080',int64)) == 0
    end function is8d

    pure elemental integer function p8sw(val) result(res)
        integer(int64), intent(in) :: val
        integer(int64) :: v
        integer(int64), parameter :: m  = int(z'000000FF000000FF', int64)
        integer(int64), parameter :: m1 = int(z'000F424000000064', int64)
        integer(int64), parameter :: m2 = int(z'0000271000000001', int64)
        v   = val - int(z'3030303030303030', int64)
        v   = v*10 + ishft(v, -8)
        v   = ishft(iand(v,m)*m1+iand(ishft(v,-16),m)*m2,-32)
        res = int(iand(v, int(z'FFFFFFFF',int64)), int32)
    end function p8sw

    pure elemental subroutine lp8(pos, last, str, i)
        integer, intent(in) :: last
        integer, intent(inout) :: pos
        character(len=*), intent(in) :: str
        integer(int64), intent(inout) :: i
        integer(int64) :: val
        do while (last - pos + 1 >= 8)
            val = r8(str(pos:))
            if (.not. is8d(val)) exit
            i = i * 100000000_int64 + int(p8sw(val), int64)
            pos = pos + 8
        end do
    end subroutine lp8

    pure logical function cc3(str, e3)
        character(3), intent(in) :: str
        character(3), intent(in) :: e3
        integer :: i
        cc3 = .true.
        do i = 0, 2
            if (ior(iachar(str(i+1:i+1)),32) /= iachar(e3(i+1:i+1))) then
                cc3 = .false.
                return
            end if
        end do
    end function cc3

    pure logical function cc5(str, e5)
        character(5), intent(in) :: str
        character(5), intent(in) :: e5
        integer :: i
        cc5 = .true.
        do i = 0, 4
            if (ior(iachar(str(i+1:i+1)),32) /= &
                iachar(e5(i+1:i+1))) then
                cc5 = .false.; return
            end if
        end do
    end function cc5

    ! ===== Parse number string =====
    elemental subroutine pns(first, last, str, opts, bj, a)
        integer, intent(in) :: first, last
        character(*), intent(in) :: str
        type(ffc_parse_options), intent(in) :: opts
        logical, intent(in) :: bj
        type(fparsed), intent(out) :: a

        integer(int64) :: fmt,i,dc,exp,en
        integer(int64), parameter :: m19 = 1000000000000000000_int64
        character :: dp
        integer :: p,sd,eip,bf,le,ic,ie,fe,sp
        logical :: alp,hdp,ne,hse,hed

        fmt=opts%format; dp=opts%decimal_point
        p=first; a%valid=.false.
        if (p>last) then; a%lastm=p; return; end if

        a%neg = (str(p:p)=='-')
        alp = iand(fmt, FMT_PLUS)/=0

        if (str(p:p)=='-' .or. &
            (alp.and..not.bj.and.str(p:p)=='+')) then
            p=p+1
            if (p>last) then; a%lastm=p; return; end if
            if (bj) then
                if (.not.isd(str(p:p))) then
                    a%lastm=p; return
                end if
            else
                if (.not.isd(str(p:p)).and.str(p:p)/=dp) &
                    then; a%lastm=p; return; end if
            end if
        end if

        sd=p; i=0_int64
        do while (p<=last)
            if (.not.isd(str(p:p))) exit
            ic = iachar(str(p:p))-48
            i = 10*i + ic; p=p+1
        end do

        eip=p; dc=int(eip-sd,int64)
        a%ips=sd; a%ipl=int(dc)

        if (bj) then
            if (dc==0) then; a%lastm=p; return; end if
            if (str(sd:sd)=='0'.and.dc>1) then
                a%lastm=sd; return
            end if
        end if

        exp=0_int64
        hdp = .false.
        if (p<=last) hdp = (str(p:p)==dp)

        if (hdp) then
            p=p+1; bf=p
            call lp8(p, last, str, i)
            do while (p<=last)
                if (.not.isd(str(p:p))) exit
                ic=iachar(str(p:p))-48
                i=i*10+int(ic,int64); p=p+1
            end do
            exp=int(bf-p,int64)
            a%fps=bf; a%fpl=p-bf; dc=dc-exp
        end if

        if (bj) then
            if (hdp.and.exp==0) then
                a%lastm=p; return
            end if
        else
            if (dc==0) then; a%lastm=p; return; end if
        end if

        en=0_int64; hse=.false.
        if (p<=last) then
            hse = (iand(fmt,FMT_SCI)/=0 .and. &
                   (str(p:p)=='e'.or.str(p:p)=='E')) .or. &
                  (iand(fmt,FMT_FORT)/=0 .and. &
                   (str(p:p)=='+'.or.str(p:p)=='-' .or. &
                    str(p:p)=='d'.or.str(p:p)=='D'))
        end if
        if (hse) then
            le=p
            if (str(p:p)=='e'.or.str(p:p)=='E'.or. &
                str(p:p)=='d'.or.str(p:p)=='D') p=p+1
            ne=.false.
            if (p<=last) then
                if (str(p:p)=='-') then
                    ne=.true.; p=p+1
                else if (str(p:p)=='+') then
                    p=p+1
                end if
            end if
            hed=.false.
            if (p<=last) hed=isd(str(p:p))
            if (.not.hed) then
                if (iand(fmt,FMT_FIX)==0) then
                    a%lastm=p; return
                end if
                p=le
            else
                do while (p<=last)
                    if (.not.isd(str(p:p))) exit
                    ic=iachar(str(p:p))-48
                    if (en<268435456_int64) &
                        en = 10*en + int(ic,int64)
                    p=p+1
                end do
                if (ne) en=-en; exp=exp+en
            end if
        else
            if (iand(fmt,FMT_SCI)/=0 .and. &
                iand(fmt,FMT_FIX)==0) then
                a%lastm=p; return
            end if
        end if

        a%lastm=p; a%valid=.true.

        if (dc > 19) then
            sp=sd
            do while (sp<=last)
                if (str(sp:sp)/='0'.and.str(sp:sp)/=dp) &
                    exit
                if (str(sp:sp)=='0') dc=dc-1
                sp=sp+1
            end do
            if (dc>19) then
                a%tmd=.true.; i=0; p=a%ips
                ie=p+a%ipl
                do while (ult(i,m19).and.p<ie)
                    i = i*10 + int( &
                        iachar(str(p:p))-48, int64)
                    p=p+1
                end do
                if (uge(i,m19)) then
                    exp = int(eip-p,int64) + en
                else
                    p=a%fps; fe=p+a%fpl
                    do while (ult(i,m19).and.p<fe)
                        i = i*10 + int( iachar(str(p:p))-48, int64)
                        p=p+1
                    end do
                    exp = int(a%fps-p,int64) + en
                end if
            end if
        end if
        a%exponent=exp; a%mantissa=i
    end subroutine pns

    ! ===== Parse inf/nan =====
    elemental subroutine pin_32(str,p0,la,vf,res)
        character(*), intent(in) :: str
        integer, intent(in) :: p0, la
        real(real32), intent(out) :: vf
        type(ffc_result), intent(out) :: res
        integer :: p, pp
        logical :: ms
        if (p0>la) then
            res = ffc_result(p0,FFC_OUTCOME_INVALID_INPUT)
            return
        else
            res = ffc_result(p0,FFC_OUTCOME_OK)
        end if
        p=p0
        ms=str(p:p)=='-'
        if (ms.or.str(p:p)=='+') p=p+1
        if (la-p+1>=3) then
            if (cc3(str(p:),'nan')) then
                p=p+3; res%pos=p
                vf=ieee_value(0.0_real32,ieee_quiet_nan)
                if (ms) vf=-vf
                if (p<=la) then
                    if (str(p:p)=='(') then
                        pp=p+1
                        do while (pp<=la)
                            if (str(pp:pp)==')') then
                                res%pos=pp+1; exit
                            end if; pp=pp+1
                        end do
                    end if
                end if; return
            end if
            if (cc3(str(p:),'inf')) then
                res%pos=p+3
                if (la-p+1>=8) then
                    if (cc5(str(p+3:),'inity')) res%pos=p+8
                end if
                vf = ieee_value(0.0_real32, ieee_positive_inf)
                if (ms) vf=-vf
                return
            end if
        end if
        res%outcome=FFC_OUTCOME_INVALID_INPUT
    end subroutine pin_32

    elemental subroutine pin_64(str,p0,la,vd,res)
        character(*), intent(in) :: str
        integer, intent(in) :: p0, la
        real(real64), intent(out) :: vd
        type(ffc_result), intent(out) :: res
        integer :: p,pp
        logical :: ms        
        res%pos=p0        
        if (p0>la) then
            res%outcome=FFC_OUTCOME_INVALID_INPUT
            return
        else
            res%outcome=FFC_OUTCOME_OK
        end if
        p=p0
        ms=(str(p:p)=='-')
        if (str(p:p)=='-'.or.str(p:p)=='+') p=p+1
        if (la-p+1>=3) then
            if (cc3(str(p:),'nan')) then
                p=p+3; res%pos=p
                vd=ieee_value(0.0_real64,ieee_quiet_nan)
                if (ms) vd=-vd
                if (p<=la) then
                    if (str(p:p)=='(') then
                    pp=p+1
                        do while (pp<=la)
                            if (str(pp:pp)==')') then
                                res%pos=pp+1
                                exit
                            end if
                            pp=pp+1
                        end do
                    end if
                end if
                return
            end if
            if (cc3(str(p:),'inf')) then
                res%pos=p+3
                if (la-p+1>=8) then
                    if (cc5(str(p+3:),'inity')) res%pos=p+8
                end if
                vd = ieee_value(0.0_real64, ieee_positive_inf)
                if (ms) vd=-vd
                return
            end if
        end if
        res%outcome=FFC_OUTCOME_INVALID_INPUT
    end subroutine pin_64

    ! ===== Eisel-Lemire =====
    pure elemental integer(int32) function b2b(q)
        integer(int32), intent(in) :: q
        b2b = int(ishft((152170+65536)*int(q,int64),-16),int32)+63
    end function b2b

    pure elemental subroutine cprd(q, w, f, res)
        integer(int64), intent(in) :: q, w
        type(ifmt), intent(in) :: f
        type(u128), intent(out) :: res
        integer(int64) :: pm, bp
        integer :: idx
        type(u128) :: sp
        bp = int(f%meb+3, int64)
        pm = ishft(not(0_int64), -int(bp))
        idx = 2*int(q-(-342_int64))+1
        call mu64(w, P5(idx), res)
        if (iand(res%hi,pm)==pm) then
            call mu64(w, P5(idx+1), sp)
            res%lo = res%lo + sp%hi
            if (ult(res%lo, sp%hi)) res%hi = res%hi + 1
        end if
    end subroutine cprd

    pure elemental subroutine cflt(q, wi, f, res)
        integer(int64), intent(in) :: q, wi
        type(ifmt), intent(in) :: f
        type(fam), intent(out) :: res
        integer(int64) :: w
        integer(int32) :: lz
        type(u128) :: pr
        integer :: ub, sa

        w=wi
        if (w==0.or.q<int(f%smp10,int64)) then
            res=fam(0_int64,0_int32)
            return
        end if
        if (q>int(f%lgp10,int64)) then
            res=fam(0_int64,int(f%infp,int32))
            return
        end if
        lz=int(clz(w),int32); w=ishft(w,lz)
        call cprd(q,w,f,pr)
        ub=int(ishft(pr%hi,-63)); sa=ub+64-f%meb-3
        res%mantissa=ishft(pr%hi,-sa)
        res%power2=b2b(int(q,int32))+int(ub,int32)- &
            lz-int(f%mine,int32)

        if (res%power2<=0) then
            if (-res%power2+1>=64) then
                res=fam(0_int64,0_int32)
                return
            end if
            res%mantissa=ishft(res%mantissa, &
                               res%power2-1_int32)
            res%mantissa=res%mantissa+ &
                iand(res%mantissa,1_int64)
            res%mantissa=ishft(res%mantissa,-1)
            if (ult(res%mantissa, &
                    ishft(1_int64,f%meb))) then
                res%power2=0
            else
                res%power2=1
            end if; return
        end if

        if (iand(pr%lo,not(1_int64))==0 .and. &
            q>=int(f%minrte,int64) .and. &
            q<=int(f%maxrte,int64) .and. &
            iand(res%mantissa,3_int64)==1) then
            if (ishft(res%mantissa,sa)==pr%hi) &
                res%mantissa=iand(res%mantissa, &
                                  not(1_int64))
        end if

        res%mantissa=res%mantissa+ iand(res%mantissa,1_int64)
        res%mantissa=ishft(res%mantissa,-1)

        if (uge(res%mantissa,ishft(2_int64,f%meb))) then
            res%mantissa=ishft(1_int64,f%meb)
            res%power2=res%power2+1
        end if

        res%mantissa=iand(res%mantissa, &
            not(ishft(1_int64,f%meb)))
        if (res%power2>=int(f%infp,int32)) then
            res%power2=int(f%infp,int32)
            res%mantissa=0
        end if
    end subroutine cflt

    pure elemental subroutine cerrs(q, wi, lz, f, res)
        integer(int64), intent(in) :: q, wi
        integer(int32), intent(in) :: lz
        type(ifmt), intent(in) :: f
        type(fam), intent(out) :: res
        integer :: h, b
        h = int(ieor(ishft(wi,-63),1_int64))
        res%mantissa = ishft(wi, h)
        b = f%meb - f%mine
        res%power2 = b2b(int(q,int32))+int(b,int32)- &
            int(h,int32)-lz-62+INVALID_AM
    end subroutine cerrs

    pure elemental subroutine cerr(q, wi, f, res)
        integer(int64), intent(in) :: q, wi
        type(ifmt), intent(in) :: f
        type(fam), intent(out) :: res
        integer(int64) :: w
        integer(int32) :: lz
        type(u128) :: pr
        w=wi; lz=int(clz(w),int32); w=ishft(w,lz)
        call cprd(q,w,f,pr)
        call cerrs(q, pr%hi, lz, f, res)
    end subroutine cerr

    pure elemental subroutine clfp_64(m,e,ng,vd,f,ok)
        integer(int64), intent(in) :: m, e
        logical, intent(in) :: ng
        real(real64), intent(inout) :: vd
        type(ifmt), intent(in) :: f
        logical, intent(out) :: ok

        ok = e>=int(f%minfp,int64) .and. e<=int(f%maxfp,int64) .and. m<=f%maxm
        if (.not.ok) return

        if (e<0) then
            vd = real(m/DPOW10(-e),real64)
        else
            vd = real(m*DPOW10(e),real64)
        end if
        if (ng) vd = -vd

    end subroutine clfp_64

    pure elemental subroutine clfp_32(m,e,ng,vf,f,ok)
        integer(int64), intent(in) :: m, e
        logical, intent(in) :: ng
        real(real32), intent(inout) :: vf
        type(ifmt), intent(in) :: f
        logical, intent(out) :: ok

        ok = e>=int(f%minfp,int64) .and. e<=int(f%maxfp,int64) .and. m<=f%maxm
        if (.not.ok) return

        if (e<0) then
            vf = real(m,real32)/FPOW10(-e)
        else
            vf = real(m,real32)*FPOW10(e)
        end if
        if (ng) vf = -vf

    end subroutine clfp_32

    pure elemental real(real64) function a2d(ng, am) result(v)
        logical, intent(in) :: ng
        type(fam), intent(in) :: am
        integer(int64) :: w
        w=am%mantissa
        w=ior(w,ishft(int(am%power2,int64),52))
        if (ng) w=ior(w,ishft(1_int64,63))
        v=transfer(w,0.0_real64)
    end function a2d

    pure elemental real(real32) function a2f(ng, am) result(v)
        logical, intent(in) :: ng
        type(fam), intent(in) :: am
        integer(int32) :: w
        w=int(am%mantissa,int32)
        w=ior(w,ishft(am%power2,23))
        if (ng) w=ior(w,ishft(1_int32,31))
        v=transfer(w,0.0_real32)
    end function a2f

    ! ===== Bigint ops =====
    pure elemental subroutine svp(sv,v)
        type(fsv), intent(inout) :: sv
        integer(int64), intent(in) :: v
        sv%ln=sv%ln+1; sv%d(sv%ln)=v
    end subroutine svp

    pure elemental subroutine svtp(sv,v,ok)
        type(fsv), intent(inout) :: sv
        integer(int64), intent(in) :: v
        logical, intent(out) :: ok
        if (sv%ln<SVLC) then
            sv%ln=sv%ln+1; sv%d(sv%ln)=v; ok=.true.
        else; ok=.false.; end if
    end subroutine svtp

    pure elemental subroutine svtr(sv,nl,ok)
        type(fsv), intent(inout) :: sv
        integer, intent(in) :: nl
        logical, intent(out) :: ok
        integer :: i
        if (nl>SVLC) then; ok=.false.; return; end if
        if (nl>sv%ln) then
            do i=sv%ln+1,nl; sv%d(i)=0; end do
        end if; sv%ln=nl; ok=.true.
    end subroutine svtr

    pure elemental subroutine svn(sv)
        type(fsv), intent(inout) :: sv
        do while (sv%ln>0)
            if (sv%d(sv%ln)/=0) exit; sv%ln=sv%ln-1
        end do
    end subroutine svn

    pure elemental integer(int64) function svri(sv,i)
        type(fsv), intent(in) :: sv
        integer, intent(in) :: i
        svri = sv%d(sv%ln-i)
    end function svri

    pure elemental logical function svnza(sv,i)
        type(fsv), intent(in) :: sv
        integer, intent(in) :: i
        integer :: j
        svnza=.false.
        do j=i,sv%ln-1
            if (sv%d(sv%ln-j)/=0) then
                svnza=.true.; return
            end if
        end do
    end function svnza

    pure elemental subroutine sca(x,y,r,ov)
        integer(int64), intent(in) :: x,y
        integer(int64), intent(out) :: r
        logical, intent(out) :: ov
        integer(int64) :: s
        s=x+y; ov=ult(s,x); r=s
    end subroutine sca

    pure elemental subroutine scm(x,y,c,r)
        integer(int64), intent(in) :: x,y
        integer(int64), intent(inout) :: c
        integer(int64), intent(out) :: r
        type(u128) :: z; logical :: ov; integer(int64) :: t
        call mu64(x,y,z); call sca(z%lo,c,t,ov)
        z%lo=t; if (ov) z%hi=z%hi+1; c=z%hi; r=z%lo
    end subroutine scm

    pure elemental subroutine bsa(sv,y,st,ok)
        type(fsv), intent(inout) :: sv
        integer(int64), intent(in) :: y
        integer, intent(in) :: st
        logical, intent(out) :: ok
        integer :: j; integer(int64) :: c,t; logical :: ov
        c=y; j=st
        do while (c/=0.and.j<=sv%ln)
            call sca(sv%d(j),c,t,ov); sv%d(j)=t
            c=merge(1_int64,0_int64,ov); j=j+1
        end do
        if (c/=0) then; call svtp(sv,c,ok); return; end if
        ok=.true.
    end subroutine bsa

    pure elemental subroutine bsm(sv,y,ok)
        type(fsv), intent(inout) :: sv
        integer(int64), intent(in) :: y
        logical, intent(out) :: ok
        integer :: j; integer(int64) :: c,t
        c=0
        do j=1,sv%ln
            call scm(sv%d(j),y,c,t); sv%d(j)=t
        end do
        if (c/=0) then; call svtp(sv,c,ok); return; end if
        ok=.true.
    end subroutine bsm

    pure subroutine bla(x,yd,yl,st,bla_ok)
        type(fsv), intent(inout) :: x
        integer(int64), intent(in) :: yd(:)
        integer, intent(in) :: yl,st
        logical, intent(out) :: bla_ok
        integer :: j; logical :: cf,c1,c2,ok
        integer(int64) :: xi,yi,t,t2
        if (x%ln<st-1+yl) then
            call svtr(x,st-1+yl,ok)
            if (.not.ok) then; bla_ok=.false.; return; end if
        end if; cf=.false.
        do j=1,yl
            xi=x%d(st-1+j); yi=yd(j)
            call sca(xi,yi,t,c1); c2=.false.
            if (cf) then; call sca(t,1_int64,t2,c2); t=t2; end if
            x%d(st-1+j)=t; cf=c1.or.c2
        end do
        if (cf) then
            call bsa(x,1_int64,st+yl,bla_ok); return
        end if; bla_ok=.true.
    end subroutine bla

    pure subroutine blm(x,yd,yl,blm_ok)
        type(fsv), intent(inout) :: x
        integer(int64), intent(in) :: yd(:)
        integer, intent(in) :: yl
        logical, intent(out) :: blm_ok
        type(fsv) :: z,zi; integer :: j
        integer(int64) :: yi; logical :: ok
        z%ln=x%ln; z%d(1:x%ln)=x%d(1:x%ln)
        if (yl/=0) then
            call bsm(x,yd(1),ok)
            if (.not.ok) then; blm_ok=.false.; return; end if
            do j=2,yl
                yi=yd(j)
                if (yi/=0) then
                    zi%ln=z%ln
                    zi%d(1:z%ln)=z%d(1:z%ln)
                    call bsm(zi,yi,ok)
                    if (.not.ok) then
                        blm_ok=.false.; return
                    end if
                    call bla(x,zi%d,zi%ln,j,ok)
                    if (.not.ok) then
                        blm_ok=.false.; return
                    end if
                end if
            end do
        end if; call svn(x); blm_ok=.true.
    end subroutine blm

    pure subroutine blgm(x,yd,yl,ok)
        type(fsv), intent(inout) :: x
        integer(int64), intent(in) :: yd(:)
        integer, intent(in) :: yl
        logical, intent(out) :: ok
        if (yl==1) then; call bsm(x,yd(1),ok)
        else; call blm(x,yd,yl,ok); end if
    end subroutine blgm

    pure subroutine bshb(sv,n,ok)
        type(fsv), intent(inout) :: sv
        integer, intent(in) :: n
        logical, intent(out) :: ok
        integer :: sl,sr,j; integer(int64) :: pv,xi,cy
        sl=n; sr=LB-sl; pv=0
        do j=1,sv%ln
            xi=sv%d(j)
            sv%d(j)=ior(ishft(xi,sl),ishft(pv,-sr))
            pv=xi
        end do; cy=ishft(pv,-sr)
        if (cy/=0) then; call svtp(sv,cy,ok); return; end if
        ok=.true.
    end subroutine bshb

    pure subroutine bshl(sv,n,ok)
        type(fsv), intent(inout) :: sv
        integer, intent(in) :: n
        logical, intent(out) :: ok
        integer :: i
        if (n+sv%ln>SVLC) then; ok=.false.; return; end if
        if (sv%ln/=0) then
            do i=sv%ln,1,-1; sv%d(i+n)=sv%d(i); end do
            do i=1,n; sv%d(i)=0; end do
            sv%ln=sv%ln+n
        end if; ok=.true.
    end subroutine bshl

    pure subroutine bsh(sv,n,ok)
        type(fsv), intent(inout) :: sv
        integer, intent(in) :: n
        logical, intent(out) :: ok
        integer :: rm,dv
        rm=mod(n,LB); dv=n/LB; ok=.true.
        if (rm/=0) then
            call bshb(sv,rm,ok); if (.not.ok) return
        end if
        if (dv/=0) call bshl(sv,dv,ok)
    end subroutine bsh

    pure subroutine bp2(bi,e,ok)
        type(fbigint), intent(inout) :: bi
        integer, intent(in) :: e
        logical, intent(out) :: ok
        call bsh(bi%vec,e,ok)
    end subroutine bp2

    pure subroutine bp5(bi,ei,bp5_ok)
        type(fbigint), intent(inout) :: bi
        integer, intent(in) :: ei
        logical, intent(out) :: bp5_ok
        integer :: er; logical :: ok
        integer, parameter :: ss=27
        integer(int64), parameter :: mn = 7450580596923828125_int64
        er=ei; bp5_ok=.false.
        do while (er>=P5LS)
            call blgm(bi%vec,P5L,5,ok)
            if (.not.ok) return
            er=er-P5LS
        end do
        do while (er>=ss)
            call bsm(bi%vec,mn,ok)
            if (.not.ok) return
            er=er-ss
        end do
        if (er/=0) then
            call bsm(bi%vec,P5S(er),ok)
            if (.not.ok) return
        end if
        bp5_ok = .true.
    end subroutine bp5

    pure subroutine bp10(bi,e,ok)
        type(fbigint), intent(inout) :: bi
        integer, intent(in) :: e
        logical, intent(out) :: ok
        call bp5(bi,e,ok); if (ok) call bp2(bi,e,ok)
    end subroutine bp10

    pure elemental subroutine bimk(v, res)
        integer(int64), intent(in) :: v
        type(fbigint), intent(out) :: res
        res%vec%ln=0; call svp(res%vec,v)
        call svn(res%vec)
    end subroutine bimk

    pure elemental subroutine biem(res)
        type(fbigint), intent(out) :: res
        res%vec%ln=0
    end subroutine biem

    pure elemental integer function bctl(bi)
        type(fbigint), intent(in) :: bi
        if (bi%vec%ln==0) then; bctl=0
        else; bctl=clz(bi%vec%d(bi%vec%ln)); end if
    end function bctl

    pure elemental integer function bbl(bi)
        type(fbigint), intent(in) :: bi
        bbl=LB*bi%vec%ln-bctl(bi)
    end function bbl

    pure elemental subroutine h64_1(r0,tr,res)
        integer(int64), intent(in) :: r0
        logical, intent(out) :: tr
        integer(int64), intent(out) :: res
        tr=.false.; res=ishft(r0,clz(r0))
    end subroutine h64_1

    pure elemental subroutine h64_2(r0,r1,tr,res)
        integer(int64), intent(in) :: r0,r1
        logical, intent(out) :: tr
        integer(int64), intent(out) :: res
        integer :: s
        s=clz(r0)
        if (s==0) then
            tr=r1/=0; res=r0
        else
            tr=ishft(r1,s)/=0
            res=ior(ishft(r0,s),ishft(r1,-(64-s)))
        end if
    end subroutine h64_2

    pure elemental subroutine bh64(bi,tr,res)
        type(fbigint), intent(in) :: bi
        logical, intent(out) :: tr
        integer(int64), intent(out) :: res
        if (bi%vec%ln==0) then
            tr=.false.; res=0
        else if (bi%vec%ln==1) then
            call h64_1(svri(bi%vec,0),tr,res)
        else
            call h64_2(svri(bi%vec,0),svri(bi%vec,1),tr,res)
            tr=tr.or.svnza(bi%vec,2)
        end if
    end subroutine bh64

    pure elemental integer function bcmp(a,b)
        type(fbigint), intent(in) :: a,b
        integer :: j
        if (a%vec%ln>b%vec%ln) then; bcmp=1; return
        else if (a%vec%ln<b%vec%ln) then
            bcmp=-1; return
        end if; bcmp=0
        do j=a%vec%ln,1,-1
            if (ugt(a%vec%d(j),b%vec%d(j))) then
                bcmp=1; return
            else if (ult(a%vec%d(j),b%vec%d(j))) then
                bcmp=-1; return
            end if
        end do
    end function bcmp

    ! ===== Digit comparison =====
    pure elemental integer(int32) function scex(mi,e)
        integer(int64), intent(in) :: mi
        integer(int32), intent(in) :: e
        integer(int64) :: m
        m=mi; scex=e
        do while (m>=10000); m=m/10000; scex=scex+4; end do
        do while (m>=100); m=m/100; scex=scex+2; end do
        do while (m>=10); m=m/10; scex=scex+1; end do
    end function scex

    pure elemental subroutine toex(vd,f,res)
        real(real64), intent(in) :: vd
        type(ifmt), intent(in) :: f
        type(fam), intent(out) :: res
        integer(int64) :: b; integer(int32) :: bi
        bi=int(f%meb-f%mine,int32); b=gdbb(vd)
        if (iand(b,f%emask)==0) then
            res%power2=1-bi; res%mantissa=iand(b,f%mmask)
        else
            res%power2=int(ishft(iand(b,f%emask), &
                -f%meb),int32)-bi
            res%mantissa=ior(iand(b,f%mmask),f%hbm)
        end if
    end subroutine toex

    pure elemental subroutine toexh(vd,f,res)
        real(real64), intent(in) :: vd
        type(ifmt), intent(in) :: f
        type(fam), intent(out) :: res
        call toex(vd,f,res)
        res%mantissa=ishft(res%mantissa,1)+1
        res%power2=res%power2-1
    end subroutine toexh

    pure elemental subroutine rdi(am,s)
        type(fam), intent(inout) :: am
        integer(int32), intent(in) :: s
        if (s==64) then; am%mantissa=0; else
            am%mantissa=ishft(am%mantissa,-s)
        end if; am%power2=am%power2+s
    end subroutine rdi

    pure elemental subroutine rtt(am,s,tr)
        type(fam), intent(inout) :: am
        integer(int32), intent(in) :: s
        logical, intent(in) :: tr
        integer(int64) :: mk,hw,tb
        logical :: ab,hf,od
        if (s==64) then; mk=not(0_int64)
        else; mk=ishft(1_int64,s)-1; end if
        if (s==0) then; hw=0
        else; hw=ishft(1_int64,s-1); end if
        tb=iand(am%mantissa,mk)
        ab=ugt(tb,hw); hf=tb==hw
        if (s==64) then; am%mantissa=0
        else; am%mantissa=ishft(am%mantissa,-s); end if
        am%power2=am%power2+s
        od=iand(am%mantissa,1_int64)==1
        if (ab.or.(hf.and.tr).or.(od.and.hf)) &
            am%mantissa=am%mantissa+1
    end subroutine rtt

    pure elemental subroutine rtc(am,s,ord)
        type(fam), intent(inout) :: am
        integer(int32), intent(in) :: s
        integer, intent(in) :: ord
        logical :: od
        if (s==64) then; am%mantissa=0
        else; am%mantissa=ishft(am%mantissa,-s); end if
        am%power2=am%power2+s
        od=iand(am%mantissa,1_int64)==1
        if (ord>0.or.(ord==0.and.od)) &
            am%mantissa=am%mantissa+1
    end subroutine rtc

    pure elemental subroutine rfn(am,f)
        type(fam), intent(inout) :: am
        type(ifmt), intent(in) :: f
        if (uge(am%mantissa,ishft(2_int64,f%meb))) then
            am%mantissa=ishft(1_int64,f%meb)
            am%power2=am%power2+1
        end if
        am%mantissa=iand(am%mantissa, &
            not(ishft(1_int64,f%meb)))
        if (am%power2>=int(f%infp,int32)) then
            am%power2=int(f%infp,int32); am%mantissa=0
        end if
    end subroutine rfn

    pure elemental subroutine rdn(am,f)
        type(fam), intent(inout) :: am
        type(ifmt), intent(in) :: f
        integer(int32) :: ms,s
        ms=int(64-f%meb-1,int32)
        if (-am%power2>=ms) then
            s=min(-am%power2+1,64_int32)
            call rdi(am,s)
            if (ult(am%mantissa,ishft(1_int64,f%meb)))then
                am%power2=0
            else; am%power2=1; end if; return
        end if
        call rdi(am,ms); call rfn(am,f)
    end subroutine rdn

    pure elemental subroutine rtet(am,tr,f)
        type(fam), intent(inout) :: am
        logical, intent(in) :: tr
        type(ifmt), intent(in) :: f
        integer(int32) :: ms,s
        ms=int(64-f%meb-1,int32)
        if (-am%power2>=ms) then
            s=min(-am%power2+1,64_int32)
            call rtt(am,s,tr)
            if (ult(am%mantissa,ishft(1_int64,f%meb)))then
                am%power2=0
            else; am%power2=1; end if; return
        end if
        call rtt(am,ms,tr); call rfn(am,f)
    end subroutine rtet

    pure elemental subroutine rtec(am,ord,f)
        type(fam), intent(inout) :: am
        integer, intent(in) :: ord
        type(ifmt), intent(in) :: f
        integer(int32) :: ms,s
        ms=int(64-f%meb-1,int32)
        if (-am%power2>=ms) then
            s=min(-am%power2+1,64_int32)
            call rtc(am,s,ord)
            if (ult(am%mantissa,ishft(1_int64,f%meb)))then
                am%power2=0
            else; am%power2=1; end if; return
        end if
        call rtc(am,ms,ord); call rfn(am,f)
    end subroutine rtec

    pure elemental logical function istr(str,fi,la)
        character(len=*), intent(in) :: str
        integer, intent(in) :: fi,la
        istr = verify(str(fi:la),'0')>0
    end function istr

    pure elemental subroutine skz(str,p,la)
        character(*), intent(in) :: str
        integer, intent(inout) :: p
        integer, intent(in) :: la
        do while (p<=la)
            if (str(p:p)/='0') return; p=p+1
        end do
    end subroutine skz

    pure elemental subroutine an(bi,pw,v)
        type(fbigint), intent(inout) :: bi
        integer(int64), intent(in) :: pw,v
        logical :: ok
        call bsm(bi%vec,pw,ok); call bsa(bi%vec,v,1,ok)
    end subroutine an

    pure elemental subroutine rubi(bi,cn)
        type(fbigint), intent(inout) :: bi
        integer, intent(inout) :: cn
        call an(bi,10_int64,1_int64); cn=cn+1
    end subroutine rubi

    pure elemental subroutine pm(str,num,md,rb,dg)
        character(*), intent(in) :: str
        type(fparsed), intent(in) :: num
        integer, intent(in) :: md
        type(fbigint), intent(inout) :: rb
        integer, intent(out) :: dg
        integer :: ct,p,pe,stp
        integer(int64) :: v; logical :: tr

        ct=0; dg=0; v=0; stp=19

        if (num%ips>0.and.num%ipl>0) then
            p=num%ips; pe=p+num%ipl-1
            call skz(str,p,pe)
            do while (p<=pe)
                do while (pe-p+1>=8 .and. stp-ct>=8 &
                          .and. md-dg>=8)
                    v=v*100000000_int64 + &
                        int(p8sw(r8(str(p:))),int64)
                    p=p+8; ct=ct+8; dg=dg+8
                end do
                do while (ct<stp.and.p<=pe.and.dg<md)
                    v=v*10+int(iachar(str(p:p))-48,int64)
                    p=p+1; ct=ct+1; dg=dg+1
                end do
                if (dg==md) then
                    call an(rb,P10U64(ct),v)
                    tr=istr(str,p,pe)
                    if (num%fps>0.and.num%fpl>0) &
                        tr=tr.or.istr(str,num%fps, &
                            num%fps+num%fpl-1)
                    if (tr) call rubi(rb,dg); return
                else
                    call an(rb,P10U64(ct),v)
                    ct=0; v=0
                end if
            end do
        end if

        if (num%fps>0.and.num%fpl>0) then
            p=num%fps; pe=p+num%fpl-1
            if (dg==0) call skz(str,p,pe)
            do while (p<=pe)
                do while (pe-p+1>=8 .and. stp-ct>=8 &
                          .and. md-dg>=8)
                    v=v*100000000_int64 + &
                        int(p8sw(r8(str(p:))),int64)
                    p=p+8; ct=ct+8; dg=dg+8
                end do
                do while (ct<stp.and.p<=pe.and.dg<md)
                    v=v*10+int(iachar(str(p:p))-48,int64)
                    p=p+1; ct=ct+1; dg=dg+1
                end do
                if (dg==md) then
                    call an(rb,P10U64(ct),v)
                    tr=istr(str,p,pe)
                    if (tr) call rubi(rb,dg); return
                else
                    call an(rb,P10U64(ct),v)
                    ct=0; v=0
                end if
            end do
        end if
        if (ct/=0) call an(rb,P10U64(ct),v)
    end subroutine pm

    pure elemental subroutine pdc(str,bm,ev,f,res)
        character(*), intent(in) :: str
        type(fbigint), intent(inout) :: bm
        integer(int32), intent(in) :: ev
        type(ifmt), intent(in) :: f
        type(fam), intent(out) :: res
        logical :: tr,ok; integer :: bi
        call bp10(bm,int(ev),ok)
        call bh64(bm,tr,res%mantissa)
        bi=f%meb-f%mine
        res%power2=int(bbl(bm)-64+bi,int32)
        call rtet(res,tr,f)
    end subroutine pdc

    pure elemental subroutine ndc(str,bm,ai,ev,f,res)
        character(*), intent(in) :: str
        type(fbigint), intent(inout) :: bm
        type(fam), intent(in) :: ai
        integer(int32), intent(in) :: ev
        type(ifmt), intent(in) :: f
        type(fam), intent(out) :: res
        type(fam) :: ab; real(real64) :: bv
        type(fam) :: th; type(fbigint) :: td
        integer(int32) :: te,p2e; integer :: p5e,ord
        logical :: ok
        ab=ai; call rdn(ab,f); bv = a2d(.false.,ab)
        call toexh(bv,f,th); call bimk(th%mantissa,td)
        te=th%power2; p2e=te-ev; p5e=int(-ev)
        if (p5e/=0) call bp5(td,p5e,ok)
        if (p2e>0) then; call bp2(td,int(p2e),ok)
        else if (p2e<0) then; call bp2(bm,int(-p2e),ok)
        end if
        ord=bcmp(bm,td); res=ai; call rtec(res,ord,f)
    end subroutine ndc

    pure elemental subroutine dcomp(str,num,ai,f,res)
        character(*), intent(in) :: str
        type(fparsed), intent(in) :: num
        type(fam), intent(in) :: ai
        type(ifmt), intent(in) :: f
        type(fam), intent(out) :: res
        type(fam) :: am; integer(int32) :: se,ev
        integer :: dg; type(fbigint) :: bm
        am=ai; am%power2=am%power2-INVALID_AM
        se=scex(num%mantissa,int(num%exponent,int32))
        call biem(bm)
        call pm(str,num,f%maxd,bm,dg)
        ev=se+1-int(dg,int32)
        if (ev>=0) then; call pdc(str,bm,ev,f,res)
        else; call ndc(str,bm,am,ev,f,res); end if
    end subroutine dcomp

    ! ===== Specialized conversion: double precision =====
    pure elemental subroutine fchars_64(str,p,vd,f,res)
        character(*), intent(in) :: str
        type(fparsed), intent(in) :: p
        real(real64), intent(inout) :: vd
        type(ifmt), intent(in) :: f
        type(ffc_result), intent(out) :: res
        type(fam) :: am,ap; logical :: eq, cfok

        res%outcome=FFC_OUTCOME_OK
        res%pos    =p%lastm

        if (.not.p%tmd) then
            call clfp_64(p%mantissa,p%exponent,p%neg,vd,f,cfok)
            if (cfok) return
        end if

        call cflt(p%exponent,p%mantissa,f,am)
        if (p%tmd.and.am%power2>=0) then
            call cflt(p%exponent,p%mantissa+1,f,ap)
            eq=am%mantissa==ap%mantissa .and. &
               am%power2==ap%power2
            if (.not.eq) call cerr(p%exponent,p%mantissa,f,am)
        end if
        if (am%power2<0) call dcomp(str,p,am,f,am)

        vd = a2d(p%neg,am)

        if ((p%mantissa/=0.and.am%mantissa==0.and.am%power2==0) .or. &
            am%power2==int(f%infp,int32)) &
            res%outcome=FFC_OUTCOME_OUT_OF_RANGE
    end subroutine fchars_64

    ! ===== Specialized conversion: single precision =====
    pure elemental subroutine fchars_32(str,p,vf,f,res)
        character(*), intent(in) :: str
        type(fparsed), intent(in) :: p
        real(real32), intent(inout) :: vf
        type(ifmt), intent(in) :: f
        type(ffc_result), intent(out) :: res
        type(fam) :: am,ap; logical :: eq, cfok

        res%outcome=FFC_OUTCOME_OK
        res%pos    =p%lastm

        if (.not.p%tmd) then
            call clfp_32(p%mantissa,p%exponent,p%neg,vf,f,cfok)
            if (cfok) return
        end if

        call cflt(p%exponent,p%mantissa,f,am)
        if (p%tmd.and.am%power2>=0) then
            call cflt(p%exponent,p%mantissa+1,f,ap)
            eq=am%mantissa==ap%mantissa .and. &
               am%power2==ap%power2
            if (.not.eq) call cerr(p%exponent,p%mantissa,f,am)
        end if
        if (am%power2<0) call dcomp(str,p,am,f,am)

        vf = a2f(p%neg,am)

        if ((p%mantissa/=0.and.am%mantissa==0.and.am%power2==0) .or. &
            am%power2==int(f%infp,int32)) &
            res%outcome=FFC_OUTCOME_OUT_OF_RANGE
    end subroutine fchars_32

    ! ===== PUBLIC =====
    type(ffc_result) function ffc_pd(str, out) result(res)
        character(*), intent(in) :: str
        real(real64), intent(out) :: out
        call ffc_parse_double_range_sub(str, 1, len(str), out, res, DEFAULT_PARSING)
    end function ffc_pd

    type(ffc_result) function ffc_pd_opts(str, out, options) result(res)
        character(*), intent(in) :: str
        real(real64), intent(out) :: out
        type(ffc_parse_options), intent(in) :: options
        call ffc_parse_double_range_sub(str, 1, len(str), out, res, options)
    end function ffc_pd_opts

    type(ffc_result) function ffc_pdr(str, first, last, out) result(res)
        character(*), intent(in) :: str
        integer, intent(in) :: first, last
        real(real64), intent(out) :: out
        call ffc_parse_double_range_sub(str, first, last, out, res, DEFAULT_PARSING)
    end function ffc_pdr

    function ffc_pdr_opts(str, first, last, out, options) result(res)
        character(*), intent(in) :: str
        integer, intent(in) :: first, last
        real(real64), intent(out) :: out
        type(ffc_parse_options), intent(in) :: options
        type(ffc_result) :: res
        call ffc_parse_double_range_sub(str, first, last, out, res, options)
    end function ffc_pdr_opts

    elemental subroutine ffc_parse_double_range_sub(str, first, last, out, res, o)
        character(len=*), intent(in) :: str
        integer, value :: first
        integer, intent(in), value :: last
        real(real64), intent(out) :: out
        type(ffc_result), intent(out) :: res
        type(ffc_parse_options), intent(in) :: o
        type(fparsed) :: p
        logical :: bj
        
        if (iand(o%format,FMT_SKIP)/=0) then
            do while (first<=last)
                if (.not.issp(str(first:first))) exit
                first=first+1
            end do
        end if
        if (first>last) then
            res = ffc_result(first,FFC_OUTCOME_INVALID_INPUT)
            out = 0.0_real64
            return
        end if
        bj = iand(o%format,FMT_JSON)/=0
        call try_fast(first,last,str,o,bj,p)
        if (.not.p%valid) call pns(first,last,str,o,bj,p)
        if (.not.p%valid) then
            if (iand(o%format,FMT_NOIN)/=0) then
                res = ffc_result(first,FFC_OUTCOME_INVALID_INPUT)
                out = 0.0_real64
            else
                call pin_64(str,first,last,out,res)                
            end if
        else
            call fchars(str,p,out,DF,res)
        end if        
    end subroutine ffc_parse_double_range_sub

    function ffc_pf(str, out) result(res)
        character(*), intent(in) :: str
        real(real32), intent(out) :: out
        type(ffc_result) :: res
        res = ffc_pf_opts(str, out, DEFAULT_PARSING)
    end function ffc_pf

    function ffc_pf_opts(str, out, options) result(res)
        character(*), intent(in) :: str
        real(real32), intent(out) :: out
        type(ffc_parse_options), intent(in) :: options
        type(ffc_result) :: res
        type(fparsed) :: p
        integer :: ps, la; logical :: bj

        la=len(str); ps=1
        if (iand(options%format,FMT_SKIP)/=0) then
            do while (ps<=la)
                if (.not.issp(str(ps:ps))) exit; ps=ps+1
            end do
        end if
        if (ps>la) then
            res%outcome=FFC_OUTCOME_INVALID_INPUT
            res%pos=ps; return
        end if
        bj=iand(options%format,FMT_JSON)/=0
        call pns(ps,la,str,options,bj,p)
        if (.not.p%valid) then
            if (iand(options%format,FMT_NOIN)/=0) then
                res%outcome=FFC_OUTCOME_INVALID_INPUT
                res%pos=ps; return
            else
                call pin_32(str,ps,la,out,res)
                return
            end if
        end if
        call fchars(str,p,out,FF,res)
    end function ffc_pf_opts

    function ffc_parse_i64(str,base,out) result(res)
        character(*), intent(in) :: str
        integer, intent(in) :: base
        integer(int64), intent(out) :: out
        type(ffc_result) :: res
        integer :: p,la,sn,sd,dc,md,d
        logical :: ng,hlz
        integer(int64) :: i

        out=0; la=len(str); p=1
        if (p>la.or.base<2.or.base>36) then
            res%outcome=FFC_OUTCOME_INVALID_INPUT
            res%pos=p; return
        end if
        ng = str(p:p)=='-'
        if (ng) p=p+1
        sn=p
        do while (p<=la)
            if (str(p:p)/='0') exit; p=p+1
        end do
        hlz=p>sn; sd=p; i=0
        if (base==10) call lp8(p,la,str,i)
        do while (p<=la)
            d=c2dg(str(p:p)); if (d>=base) exit
            i=int(base,int64)*i+int(d,int64); p=p+1
        end do
        dc=p-sd
        if (dc==0) then
            if (hlz) then
                out=0; res%outcome=FFC_OUTCOME_OK; res%pos=p
            else
                res%outcome=FFC_OUTCOME_INVALID_INPUT
                res%pos=1
            end if; return
        end if
        res%pos=p; md=MXDG64(base)
        if (dc>md) then
            res%outcome=FFC_OUTCOME_OUT_OF_RANGE; return
        end if
        if (dc==md.and.ult(i,MSAFE64(base))) then
            res%outcome=FFC_OUTCOME_OUT_OF_RANGE; return
        end if
        if (.not.ng) then
            if (ugt(i,int(z'7FFFFFFFFFFFFFFF',int64))) then
                res%outcome=FFC_OUTCOME_OUT_OF_RANGE; return
            end if; out=i
        else
            if (ugt(i,ishft(1_int64,63))) then
                res%outcome=FFC_OUTCOME_OUT_OF_RANGE; return
            end if; out=not(i)+1
        end if
        res%outcome=FFC_OUTCOME_OK
    end function ffc_parse_i64

    function ffc_parse_i32(str,base,out) result(res)
        character(*), intent(in) :: str
        integer, intent(in) :: base
        integer(int32), intent(out) :: out
        type(ffc_result) :: res
        integer(int64) :: v
        out=0; res=ffc_parse_i64(str,base,v)
        if (res%outcome/=FFC_OUTCOME_OK) return
        if (v>int(z'7FFFFFFF',int64) .or. &
            v<int(z'FFFFFFFF80000000',int64)) then
            res%outcome=FFC_OUTCOME_OUT_OF_RANGE; return
        end if; out=int(v,int32)
    end function ffc_parse_i32

end module fast_float_module
