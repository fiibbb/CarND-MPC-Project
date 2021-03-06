*> \brief \b ILACLR
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at 
*            http://www.netlib.org/lapack/explore-html/ 
*
*> \htmlonly
*> Download ILACLR + dependencies 
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/ilaclr.f"> 
*> [TGZ]</a> 
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/ilaclr.f"> 
*> [ZIP]</a> 
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/ilaclr.f"> 
*> [TXT]</a>
*> \endhtmlonly 
*
*  Definition:
*  ===========
*
*       INTEGER FUNCTION ILACLR( M, N, a, LDA )
* 
*       .. Scalar Arguments ..
*       INTEGER            M, N, LDA
*       ..
*       .. Array Arguments ..
*       COMPLEX            a( LDA, * )
*       ..
*  
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> ILACLR scans a for its last non-zero row.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix a.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix a.
*> \endverbatim
*>
*> \param[in] a
*> \verbatim
*>          a is array, dimension (LDA,N)
*>          The m by n matrix a.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array a. LDA >= max(1,M).
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee 
*> \author Univ. of California Berkeley 
*> \author Univ. of Colorado Denver 
*> \author NAG Ltd. 
*
*> \date April 2012
*
*> \ingroup complexOTHERauxiliary
*
*  =====================================================================
      INTEGER FUNCTION ILACLR( M, N, a, LDA )
*
*  -- LAPACK auxiliary routine (version 3.4.1) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     April 2012
*
*     .. Scalar Arguments ..
      INTEGER            M, N, LDA
*     ..
*     .. Array Arguments ..
      COMPLEX            a( LDA, * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX          ZERO
      PARAMETER ( ZERO = (0.0E+0, 0.0E+0) )
*     ..
*     .. Local Scalars ..
      INTEGER I, J
*     ..
*     .. Executable Statements ..
*
*     Quick test for the common case where one corner is non-zero.
      IF( M.EQ.0 ) THEN
         ILACLR = M
      ELSE IF( a(M, 1).NE.ZERO .OR. a(M, N).NE.ZERO ) THEN
         ILACLR = M
      ELSE
*     Scan up each column tracking the last zero row seen.
         ILACLR = 0
         DO J = 1, N
            I=M
            DO WHILE((a(MAX(I,1),J).EQ.ZERO).AND.(I.GE.1))
               I=I-1
            ENDDO
            ILACLR = MAX( ILACLR, I )
         END DO
      END IF
      RETURN
      END
