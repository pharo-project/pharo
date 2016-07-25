private-extensions
extensionPragmas
	| pragmas |
	pragmas := Dictionary new.
	#(
		flExtensionOf:
		flExtensionOf:selector:
		flExtensionOf:selector:isSource:
		flExtensionOf:isSource:) do: [ :selector |
			| stop |
			stop := false.
			self class withAllSuperclassesDo: [ :class |
				stop ifFalse: [
					Pragma withPragmasIn: class do:  [ :pragma |
						(pragma keyword = selector and: [
							"don't collect overridden methods"
							(pragmas includesKey: pragma selector) not ]) ifTrue: [
								pragmas
									at: pragma selector
									put: pragma ] ].
						FLPlatform class = class ifTrue: [ stop := true ] ] ] ].
			
	^ pragmas values