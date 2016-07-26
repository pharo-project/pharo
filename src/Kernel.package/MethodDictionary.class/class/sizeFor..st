sizing
sizeFor: numberOfElements
    "Return the minimum capacity of a dictionary that can hold numberOfElements elements. At least 25% of the array must be empty and the return value must be a nonnegative power of 2. Notice that the max: 1 is because a MethodDictionaries can never be entirely empty, as the #grow method requires it not to be (since it does self basicSize * 2)"

	^(numberOfElements * 4 // 3 max: 1) asLargerPowerOfTwo 