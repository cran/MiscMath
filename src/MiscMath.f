
        Subroutine gcd(n, ints, g)

        integer I, J, K, g, n, TMP, ints(n)
       
        I = ints(1)
        J = ints(2)

        DO K = 2, n
        TMP = MIN(I, J)
        DO WHILE (TMP .NE. 0)            
           g = TMP
           TMP = MOD(J, I)
           J = MAX(TMP, I)
           I = MIN(TMP, I)
        END DO
        IF (K .LT. N) THEN
            I = g
            J = ints(K+1)
        ENDIF
        END DO   


        RETURN
        END
