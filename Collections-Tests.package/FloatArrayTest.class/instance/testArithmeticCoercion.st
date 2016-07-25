tests - arithmetic
testArithmeticCoercion
	"This test is related to http://bugs.squeak.org/view.php?id=6782"
	
	self should: [3.0 / (FloatArray with: 2.0) = (FloatArray with: 1.5)].
	self should: [3.0 * (FloatArray with: 2.0) = (FloatArray with: 6.0)].
	self should: [3.0 + (FloatArray with: 2.0) = (FloatArray with: 5.0)].
	self should: [3.0 - (FloatArray with: 2.0) = (FloatArray with: 1.0)].