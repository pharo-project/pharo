Extension { #name : 'OCUndeclaredVariableNotice' }

{ #category : '*OpalCompiler-UI' }
OCUndeclaredVariableNotice >> reparator [

	^ OCCodeReparator new node: self node
]
