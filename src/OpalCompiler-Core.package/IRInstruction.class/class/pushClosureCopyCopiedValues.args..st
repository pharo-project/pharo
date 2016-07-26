instance creation
pushClosureCopyCopiedValues: copiedValues args: arguments
	^IRPushClosureCopy new
			copiedValues: copiedValues;
			arguments: arguments;
			yourself.