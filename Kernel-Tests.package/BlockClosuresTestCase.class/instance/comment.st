jensen device examples
comment

  "  The Jensen device was something very sophisticated in the days of Algol 60.
Essentially it was tricky use of a parameter passing policy that was called 'call by name'. In modern terminology, a call by name parameter was a pair of blocks (in a system with full block closures, of course.)

For the lovers of Algol 60, here is a short example:

 BEGIN
  REAL PROCEDURE JensenSum (A, I, N);
     REAL  A;   INTEGER   I, N;
  BEGIN
    REAL  S;
    S := 0.0;
    FOR I := 1 STEP 1 UNTIL N DO  S := S + A;
  JensenSum := S;
  END;

  ARRAY X [1:10], Y[1:10, 1:10];
  COMMENT Do array initialization here ;

  JensenSum (X[I], I, 10);
  JensenSum (Y[I, I], I, 10);
  JensenSum(JensenSum(Y[I, J], J, 10), I, 10);
END;

The first call sums the elements of X, the second sums the diagonal elements of Y and the third call sums up all elements of Y.

It is possible to reimplement all this with blocks only and that is what is done in the jensen device examples.

Additional remark:
The Jensen device was something for clever minds. I remember an artice written by Donald Knuth and published in the Communications of the ACM (I think in 1962, but I may err) about that programming trick. That article showed how a simple procedure (called the general problem solver) could be used to do almost anything. The problem was of course to find out the right parameters. I seached my collection of photocopies for that article, but regrettably I could not find it. Perhaps I can find it later.
 "