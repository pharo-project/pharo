"
I am responsible of detecting when a class changes its metaclass.
"
Class {
	#name : 'ShMetaclassChangeDetector',
	#superclass : 'ShAbstractClassChangeDetector',
	#category : 'Shift-ClassBuilder-Changes',
	#package : 'Shift-ClassBuilder',
	#tag : 'Changes'
}

{ #category : 'initialization' }
ShMetaclassChangeDetector >> initialize [
	super initialize.
	builderAccessor := [ :e | e metaclassClass ].
	classAccessor := [ :e | e class class ].
	comparer := [ :a :b |  a name = b name ]
]

{ #category : 'changes' }
ShMetaclassChangeDetector >> newChanges [

	^ {(ShInstanceShapeChanged new
		builder: builder;
		yourself)}
]
