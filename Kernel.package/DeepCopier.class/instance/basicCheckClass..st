checking
basicCheckClass: aClass
	"Every class that implements veryDeepInner: or veryDeepCopyWith: must copy all its inst vars.
	The danger is that a user will add a new instance variable and forget to copy it.
	
	This method will check those conditions on a class and return a collection of methods that looked suspicious.
	If you get a non-empty collection, an implementation of veryDeepCopyWith: or veryDeepInner: needs to be updated (check inside the collection).
	The idea is to catch a change while it is still in the system of the programmer who made it.
		
	This is an old method. We do not know why it is checking for the size of the method > 20, or the inclusion of the selector in the literals.
	May require a further refactor."
	| warnings |
	warnings := Set new.
	
	(aClass includesSelector: #veryDeepInner:) ifTrue: [ | veryDeepInnerMethod |
		veryDeepInnerMethod := aClass >> #veryDeepInner:.
		((self doesMethod: #veryDeepInner: writeAllInstanceVariablesOfClass: aClass)) ifFalse: [
			warnings add: veryDeepInnerMethod ]].
	
	(aClass includesSelector: #veryDeepCopyWith:) ifTrue: [ | veryDeepCopyWithMethod |
		veryDeepCopyWithMethod := aClass >> #veryDeepCopyWith:.
		(veryDeepCopyWithMethod size > 20) & (veryDeepCopyWithMethod literals includes: #veryDeepCopyWith:) not ifTrue: [
			(self doesMethod: #veryDeepCopyWith: writeAllInstanceVariablesOfClass: aClass) ifFalse: [
				warnings add: veryDeepCopyWithMethod]]].
	
	^ warnings