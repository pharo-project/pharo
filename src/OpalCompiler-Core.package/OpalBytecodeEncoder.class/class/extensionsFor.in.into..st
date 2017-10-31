instruction stream support
extensionsFor: pc in: aCompiledMethod into: trinaryBlock
	"If the bytecode at pc is an extension, or if the bytecode at pc is preceeded by extensions,
	 then evaluate aTrinaryBlock with the values of extA and extB and number of extension *bytes*.
	 If the bytecode at pc is neither an extension or extended then evaluate with 0, 0, 0."
 
	| prevPC |
	"If there is what appears to be an extension bytecode before this bytecode
	 then scan for the previous pc to confirm."
	(pc - 2 >= aCompiledMethod initialPC
	 and: [self isExtension: (aCompiledMethod at: pc - 2)]) ifTrue:
		[prevPC := aCompiledMethod pcPreviousTo: pc.
		 (self nonExtensionPcAt: prevPC in: aCompiledMethod) = pc ifTrue:
			[^self extensionsAt: prevPC in: aCompiledMethod into: trinaryBlock]].
	^self extensionsAt: pc in: aCompiledMethod into: trinaryBlock