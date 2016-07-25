meta-actions
handlerForMetaMenu: evt
	"Return the prospective handler for invoking the meta menu. By default, the top-most morph in the innermost world gets this menu"
	self isWorldMorph ifTrue:[^self].
	evt handler ifNotNil:[evt handler isWorldMorph ifTrue:[^self]].
	^nil