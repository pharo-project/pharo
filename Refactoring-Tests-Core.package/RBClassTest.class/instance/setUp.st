set up
setUp
	| st |
	super setUp.
	st := RBNamespace new.
	objectClass := st classNamed: #Object.
	messageNodeClass := st classNamed: #RBMessageNode.
	st defineClass: 'Object subclass: #SomeClassName
	instanceVariableNames: ''instanceVariable1 instanceVariable2''
	classVariableNames: ''ClassVariable1''
	poolDictionaries: ''TextConstants''
	category: #''Refactory-Testing'''.
	newClass := st classNamed: #SomeClassName