Extension { #name : #CompiledMethod }

{ #category : #'*Keymapping-Core' }
CompiledMethod >> isShortcutDeclaration [
	^ self pragmas anySatisfy: [ :p | p selector = #shortcut ]
]
