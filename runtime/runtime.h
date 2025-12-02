/*
 * Karel Runtime Library - Header File
 */

#ifndef KAREL_RUNTIME_H
#define KAREL_RUNTIME_H

#ifdef __cplusplus
extern "C" {
#endif

// I/O Functions
void karel_print_int(int value);
void karel_print_double(double value);
void karel_print_string(const char* str);
int karel_input_int(void);
double karel_input_double(void);
void karel_input_string(char* buffer, int size);

// String Functions
int karel_strlen(const char* str);
char* karel_strcat(const char* s1, const char* s2);
char* karel_substr(const char* str, int start, int length);

// Math Functions
double karel_sqrt(double x);
double karel_sin(double x);
double karel_cos(double x);
double karel_tan(double x);
double karel_asin(double x);
double karel_acos(double x);
double karel_atan2(double y, double x);
double karel_ln(double x);
double karel_exp(double x);
double karel_abs(double x);
int karel_trunc(double x);
int karel_round(double x);

// Robot Functions
void karel_attach(void);
void karel_release(void);
void karel_hold(void);
void karel_unhold(void);
void karel_abort(void);
void karel_pause(void);
void karel_delay(int ms);
void karel_open_hand(int hand_number);
void karel_close_hand(int hand_number);
void karel_move_to(double x, double y, double z);
void karel_move_joint(int joint, double angle);

// I/O Operations
int karel_din_read(int port);
void karel_dout_write(int port, int value);
double karel_ain_read(int port);
void karel_aout_write(int port, double value);

// Error Handling
void karel_error(const char* message);
void karel_assert(int condition, const char* message);

#ifdef __cplusplus
}
#endif

#endif // KAREL_RUNTIME_H
