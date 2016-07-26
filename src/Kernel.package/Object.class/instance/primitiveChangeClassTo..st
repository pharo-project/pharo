reflective operations
primitiveChangeClassTo: anObject
	"Primitive. Change the class of the receiver into the class of the argument given that the format of the receiver matches the format of the argument's class. Fail if receiver or argument are SmallIntegers, or the receiver is an instance of a compact class and the argument isn't, or when the argument's class is compact and the receiver isn't, or when the format of the receiver is different from the format of the argument's class, or when the arguments class is fixed and the receiver's size differs from the size that an instance of the argument's class should have.
	Note: The primitive will fail in most cases that you think might work. This is mostly because of a) the difference between compact and non-compact classes, and b) because of differences in the format. As an example, '(Array new: 3) primitiveChangeClassTo: Morph basicNew' would fail for three of the reasons mentioned above. Array is compact, Morph is not (failure #1). Array is variable and Morph is fixed (different format - failure #2). Morph is a fixed-field-only object and the array is too short (failure #3).
	The facility is really provided for certain, very specific applications (mostly related to classes changing shape) and not for casual use."

	<primitive: 115>
	self primitiveFailed