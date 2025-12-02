/*
 * Karel Runtime Library
 * 
 * Provides runtime functions for Karel programs compiled to LLVM
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

// ============================================================================
// I/O Functions
// ============================================================================

/**
 * Print integer value
 */
void karel_print_int(int value) {
    printf("%d\n", value);
}

/**
 * Print double value
 */
void karel_print_double(double value) {
    printf("%.10g\n", value);
}

/**
 * Print string
 */
void karel_print_string(const char* str) {
    printf("%s\n", str);
}

/**
 * Read integer from stdin
 */
int karel_input_int(void) {
    int value;
    if (scanf("%d", &value) != 1) {
        fprintf(stderr, "Error: Failed to read integer\n");
        exit(1);
    }
    return value;
}

/**
 * Read double from stdin
 */
double karel_input_double(void) {
    double value;
    if (scanf("%lf", &value) != 1) {
        fprintf(stderr, "Error: Failed to read double\n");
        exit(1);
    }
    return value;
}

/**
 * Read string from stdin (max 256 chars)
 */
void karel_input_string(char* buffer, int size) {
    if (fgets(buffer, size, stdin) == NULL) {
        fprintf(stderr, "Error: Failed to read string\n");
        exit(1);
    }
    // Remove trailing newline
    size_t len = strlen(buffer);
    if (len > 0 && buffer[len-1] == '\n') {
        buffer[len-1] = '\0';
    }
}

// ============================================================================
// String Functions
// ============================================================================

/**
 * String length
 */
int karel_strlen(const char* str) {
    return (int)strlen(str);
}

/**
 * String concatenation (allocates new string)
 */
char* karel_strcat(const char* s1, const char* s2) {
    size_t len1 = strlen(s1);
    size_t len2 = strlen(s2);
    char* result = (char*)malloc(len1 + len2 + 1);
    if (result == NULL) {
        fprintf(stderr, "Error: Memory allocation failed\n");
        exit(1);
    }
    strcpy(result, s1);
    strcat(result, s2);
    return result;
}

/**
 * Substring (allocates new string)
 */
char* karel_substr(const char* str, int start, int length) {
    int len = (int)strlen(str);
    if (start < 0 || start >= len || length < 0) {
        return strdup("");
    }
    if (start + length > len) {
        length = len - start;
    }
    char* result = (char*)malloc(length + 1);
    if (result == NULL) {
        fprintf(stderr, "Error: Memory allocation failed\n");
        exit(1);
    }
    strncpy(result, str + start, length);
    result[length] = '\0';
    return result;
}

// ============================================================================
// Math Functions (wrappers for standard library)
// ============================================================================

double karel_sqrt(double x) {
    return sqrt(x);
}

double karel_sin(double x) {
    return sin(x);
}

double karel_cos(double x) {
    return cos(x);
}

double karel_tan(double x) {
    return tan(x);
}

double karel_asin(double x) {
    return asin(x);
}

double karel_acos(double x) {
    return acos(x);
}

double karel_atan2(double y, double x) {
    return atan2(y, x);
}

double karel_ln(double x) {
    return log(x);
}

double karel_exp(double x) {
    return exp(x);
}

double karel_abs(double x) {
    return fabs(x);
}

int karel_trunc(double x) {
    return (int)x;
}

int karel_round(double x) {
    return (int)round(x);
}

// ============================================================================
// Robot Stubs (For simulation/testing)
// ============================================================================

/**
 * ATTACH - Attach to robot group
 */
void karel_attach(void) {
    printf("[ROBOT] ATTACH\n");
}

/**
 * RELEASE - Release robot group
 */
void karel_release(void) {
    printf("[ROBOT] RELEASE\n");
}

/**
 * HOLD - Hold current position
 */
void karel_hold(void) {
    printf("[ROBOT] HOLD\n");
}

/**
 * UNHOLD - Release hold
 */
void karel_unhold(void) {
    printf("[ROBOT] UNHOLD\n");
}

/**
 * ABORT - Abort current motion
 */
void karel_abort(void) {
    printf("[ROBOT] ABORT\n");
}

/**
 * PAUSE - Pause execution
 */
void karel_pause(void) {
    printf("[ROBOT] PAUSE\n");
}

/**
 * DELAY - Delay execution (milliseconds)
 */
void karel_delay(int ms) {
    printf("[ROBOT] DELAY %d ms\n", ms);
    // In real robot: usleep(ms * 1000);
}

/**
 * Open hand
 */
void karel_open_hand(int hand_number) {
    printf("[ROBOT] OPEN HAND %d\n", hand_number);
}

/**
 * Close hand
 */
void karel_close_hand(int hand_number) {
    printf("[ROBOT] CLOSE HAND %d\n", hand_number);
}

/**
 * Move to position (stub)
 */
void karel_move_to(double x, double y, double z) {
    printf("[ROBOT] MOVE TO (%.2f, %.2f, %.2f)\n", x, y, z);
}

/**
 * Move joint (stub)
 */
void karel_move_joint(int joint, double angle) {
    printf("[ROBOT] MOVE JOINT %d to %.2f degrees\n", joint, angle);
}

// ============================================================================
// I/O Operations (Digital/Analog)
// ============================================================================

/**
 * Read digital input
 */
int karel_din_read(int port) {
    printf("[ROBOT] READ DIN[%d]\n", port);
    return 0;  // Stub
}

/**
 * Write digital output
 */
void karel_dout_write(int port, int value) {
    printf("[ROBOT] WRITE DOUT[%d] = %d\n", port, value);
}

/**
 * Read analog input
 */
double karel_ain_read(int port) {
    printf("[ROBOT] READ AIN[%d]\n", port);
    return 0.0;  // Stub
}

/**
 * Write analog output
 */
void karel_aout_write(int port, double value) {
    printf("[ROBOT] WRITE AOUT[%d] = %.2f\n", port, value);
}

// ============================================================================
// Error Handling
// ============================================================================

/**
 * Runtime error handler
 */
void karel_error(const char* message) {
    fprintf(stderr, "Karel Runtime Error: %s\n", message);
    exit(1);
}

/**
 * Assertion
 */
void karel_assert(int condition, const char* message) {
    if (!condition) {
        karel_error(message);
    }
}
