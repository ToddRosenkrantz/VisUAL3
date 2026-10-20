@ Floating-point examples for the C Call panel (VFP, hard-float calling convention).
@ Each function has saved cases, with expected results, in fp_examples.ccall:
@ open the C Call tab, pick a function and a case, and press Call.

@ C: extern float dot(const float *a, const float *b, int n);
@ C: extern double average(const double *a, int n);
@ C: extern void scale(float *a, int n, float k);
@ C: extern double hyp(double x, double y);
@ C: extern int round_to_int(float x);

        .text
        .global dot, average, scale, hyp, round_to_int

@ sum of a[i] * b[i]: a in r0, b in r1, n in r2; result in s0
dot:
        mov     r3, #0
        vmov    s0, r3                  @ sum = 0.0
        cmp     r2, #0
        beq     2f
1:      vldmia  r0!, {s2}               @ a[i], then step a
        vldmia  r1!, {s3}               @ b[i], then step b
        vmla.f32 s0, s2, s3             @ sum += a[i] * b[i]
        subs    r2, r2, #1
        bne     1b
2:      bx      lr

@ mean of n doubles: a in r0, n in r1; result in d0
average:
        vmov    s15, r1
        vcvt.f64.s32 d16, s15           @ n as a double
        mov     r2, #0
        vmov    d0, r2, r2              @ sum = 0.0
1:      vldmia  r0!, {d1}               @ a[i], then step a by 8
        vadd.f64 d0, d0, d1
        subs    r1, r1, #1
        bne     1b
        vdiv.f64 d0, d0, d16
        bx      lr

@ a[i] = a[i] * k in place: a in r0, n in r1, k in s0
scale:
        cmp     r1, #0
        beq     2f
1:      vldr    s1, [r0]
        vmul.f32 s1, s1, s0
        vstmia  r0!, {s1}               @ store a[i], then step a
        subs    r1, r1, #1
        bne     1b
2:      bx      lr

@ sqrt(x*x + y*y): x in d0, y in d1; result in d0.
@ The "overflow" case shows why library versions scale first: x*x overflows to infinity.
hyp:
        vmul.f64 d0, d0, d0             @ x*x
        vmla.f64 d0, d1, d1             @ + y*y
        vsqrt.f64 d0, d0
        bx      lr

@ round to the nearest int, ties to even (the FPSCR rounding mode): x in s0, result in r0
round_to_int:
        vcvtr.s32.f32 s0, s0
        vmov    r0, s0
        bx      lr
