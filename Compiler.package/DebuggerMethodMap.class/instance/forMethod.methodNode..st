initialize-release
forMethod: aMethod "<CompiledMethod>" methodNode: theMethodNode "<MethodNode>"
	methodReference := WeakArray with: aMethod.
	methodNode := theMethodNode.