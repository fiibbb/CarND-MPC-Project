*> \brief \b CLADIV
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at 
*            http://www.netlib.org/lapack/explore-html/ 
*
*> \htmlonly
*> Download CLADIV + dependencies 
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/cladiv.f"> 
*> [TGZ]</a> 
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/cladiv.f"> 
*> [ZIP]</a> 
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/cladiv.f"> 
*> [TXT]</a>
*> \endhtmlonly 
*
*  Definition:
*  ===========
*
*       COMPLEX FUNCTION CLADIV( x, y )
* 
*       .. Scalar Arguments ..
*       COMPLEX            x, y
*       ..
*  
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> CLADIV := x / y, where x and y are complex.  The computation of x / y
*> will not overflow on an intermediary step unless the results
*> overflows.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] x
*> \verbatim
*>          x is COMPLEX
*> \endverbatim
*>
*> \param[in] y
*> \verbatim
*>          y is COMPLEX
*>          The complex scalars x and y.
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
*> \date November 2011
*
*> \ingroup complexOTHERauxiliary
*
*  =====================================================================
      COMPLEX FUNCTION CLADIV( x, y )
*
*  -- LAPACK auxiliary routine (version 3.4.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     November 2011
*
*     .. Scalar Arguments ..
      COMPLEX            x, y
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      REAL               ZI, ZR
*     ..
*     .. External Subroutines ..
      EXTERNAL           SLADIV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          AIMAG, CMPLX, REAL
*     ..
*     .. Executable Statements ..
*
      CALL SLADIV( REAL( x ), AIMAG( x ), REAL( y ), AIMAG( y ), ZR,
     $             ZI )
      CLADIV = CMPLX( ZR, ZI )
*
      RETURN
*
*     End of CLADIV
*
      END
