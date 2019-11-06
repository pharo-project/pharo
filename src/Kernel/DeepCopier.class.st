"
DeepCopier does a veryDeepCopy.  

It is a complete tree copy using a dictionary.  Any object that is in the tree twice is only copied once.  All references to the object in the copy of the tree will point to the new copy.  See Object|veryDeepCopy which calls (self veryDeepCopyWith: aDeepCopier).

When a tree of morphs points at a morph outside of itself, that morph should not be copied.  Use our own kind of weak pointers for the 'potentially outside' morphs.   Default is that any new class will have all of its fields deeply copied.  If a field needs to be weakly copied, define veryDeepInner: and veryDeepFixupWith:.
     veryDeepInner: has the loop that actually copies the fields.  If a class defines its own copy of veryDeepInner: (to leave some fields out), then veryDeepFixupWith: will be called on that object at the end.  veryDeepInner: can compute an alternate object to put in a field.  (Object veryDeepCopyWith: discovers which superclasses did not define veryDeepInner:, and very deeply copies the variables defined in those classes).
	To decide if a class needs veryDeepInner: and veryDeepFixupWith:, ask this about an instance:  If I duplicate this object, does that mean that I also want to make duplicates of the things it holds onto?  If yes, (i.e. a Paragraph does want a new copy of its Text) then do nothing.  If no, (i.e. an undo command does not want to copy the objects it acts upon), then define veryDeepInner: and veryDeepFixupWith:.
	
Here is an analysis for the specific case of a morph being held by another morph.  
Does field X contain a morph (or a Player whose costume is a morph)?  If not, no action needed.
Is the morph in field X already a submorph of the object?  Is it down lower in the submorph tree?
	If so, no action needed.
Could the morph in field X every appear on the screen (be a submorph of some other morph)?
	If not, no action needed.
	If it could, you must write the methods veryDeepFixupWith:   and   veryDeepInner:, and in them, refrain from sending veryDeepCopyWith: to the contents of field X.


----- Things Ted is still considering -----
Rule:  If a morph stores a uniClass class (Player 57) as an object in a field, the new uniClass will not be stored there.   Each uniClass instance does have a new class created for it.  (fix this by putting the old class in references and allow lookup?  Wrong if encounter it before seeing an instance?)

Rule: If object A has object C in a field, and A says (^ C) for the copy, but object B has A in a normal field and it gets deepCopied, and A in encountered first, then there will be two copies of C.  (just be aware of it)

Dependents are now fixed up.  Suppose a model has a dependent view.  In the DependentFields dictionary, model -> (view ...).  
	If only the model is copied, no dependents are created (no one knows about the new model).  
	If only the view is copied, it is inserted into DependentFields on the right side.  model -> (view  copiedView ...).  
	If both are copied, the new model has the new view as its dependent.
	If additional things depend on a model that is copied, the caller must add them to its dependents.

"
Class {
	#name : #DeepCopier,
	#superclass : #Object,
	#instVars : [
		'references'
	],
	#category : #'Kernel-Copying'
}

{ #category : #checking }
DeepCopier >> basicCheckClass: aClass [
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
]

{ #category : #checking }
DeepCopier >> checkAllClasses [

	| warnings |
	warnings := ((self systemNavigation allClassesImplementing: #veryDeepInner:),
		(self systemNavigation allClassesImplementing: #veryDeepCopyWith:)) flatCollect: [ :aClass |
			 self basicCheckClass: aClass ].
	
	self raiseWarningsIfAny: warnings
]

{ #category : #checking }
DeepCopier >> checkClass: aClass [
	(self basicCheckClass: aClass)
		ifNotEmpty: [ :warnings | 
			Warning new
				messageText:
					'VeryDeepCopy out of sync in some classes. Some classes contain veryDeepCopyWith: or veryDeepInner: methods that are not in sync with their instance variables. Check the exception #tag for a list of them';
				tag: warnings;
				signal ]
]

{ #category : #'like fullCopy' }
DeepCopier >> checkDeep [
	"Write exceptions in the Transcript.  Every class that implements veryDeepInner: must copy all its inst vars.  Danger is that a user will add a new instance variable and forget to copy it.  This check is only run by hand once in a while to make sure nothing was forgotten.  
(Please do not remove this method.)
	DeepCopier new checkDeep 	"

	<script: 'DeepCopier new checkDeep'>
	Warning signal:
			(String
				streamContents: [ :stream | 
					stream
						nextPutAll: 'Instance variables shared with the original object when it is copied'.
					(self systemNavigation allClassesImplementing: #veryDeepInner:)
						do: [ :aClass | 
							| mm |
							(mm := aClass instVarNames size) > 0
								ifTrue: [ aClass instSize - mm + 1 to: aClass instSize do: [ :index | 
										((aClass compiledMethodAt: #veryDeepInner:) writesField: index)
											ifFalse: [ stream cr; tab.
												aClass name printOn: stream.
												stream space.
												(aClass allInstVarNames at: index) printOn: stream ] ] ] ] ])
]

{ #category : #checking }
DeepCopier >> doesMethod: aSelector writeAllInstanceVariablesOfClass: aClass [

	"This method returns true if the method of the given selector writes all instance variables of the given class.
	As an optimization, this method only checks that the last instance variable is written, assuming that all the rest are being written if the last one is."
 
	| lastFieldIndex method hasNoInstanceVariables |
	hasNoInstanceVariables := aClass instSize = 0.
	
	lastFieldIndex := aClass instSize.
	method := aClass compiledMethodAt: aSelector. 
	^ hasNoInstanceVariables or: [ method writesField: lastFieldIndex ]
]

{ #category : #'like fullCopy' }
DeepCopier >> fixDependents [
	"They are not used much, but need to be right"

	DependentsFields associationsDo:
		[:pair |
		pair value do:
			[:dep |
			(references at: dep ifAbsent: [nil]) ifNotNil:
				[:newDep| | newModel |
				newModel := references at: pair key ifAbsent: [pair key].
				newModel addDependent: newDep]]].

]

{ #category : #initialization }
DeepCopier >> initialize [

	super initialize.
	self initialize: 4096.

]

{ #category : #initialization }
DeepCopier >> initialize: size [

	references := IdentityDictionary new: size.
	
]

{ #category : #checking }
DeepCopier >> raiseWarningsIfAny: warnings [

	warnings ifNotEmpty: [
		Warning new
			messageText: 'VeryDeepCopy out of sync in some classes. Some classes contain veryDeepCopyWith: or veryDeepInner: methods that are not in sync with their instance variables. Check the exception #tag for a list of them';
			tag: warnings;
			signal ]
]

{ #category : #'like fullCopy' }
DeepCopier >> references [
	^ references
]
