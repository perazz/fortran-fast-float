#include "ffc.h"

#include <stddef.h>
#include <stdint.h>

double benchmark_ffc_lines(const char *data, const size_t *offsets, const size_t *lengths, size_t nlines, uint32_t *outcome) {
    double answer;
    double value;
    size_t i;

    answer = 0.0;
    value = 0.0;
    *outcome = FFC_OUTCOME_OK;

    for (i = 0; i < nlines; ++i) {
        ffc_result res;

        res = ffc_from_chars_double(data + offsets[i], data + offsets[i] + lengths[i], &value);
        if (res.outcome != FFC_OUTCOME_OK) {
            *outcome = res.outcome;
            continue;
        }
        answer = answer > value ? answer : value;
    }

    return answer;
}
