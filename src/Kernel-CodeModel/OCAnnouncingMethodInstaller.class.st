Class {
	#name : 'OCAnnouncingMethodInstaller',
	#superclass : 'OCMethodInstaller',
	#instVars : [
		'codeChangeAnnouncer'
	],
	#category : 'Kernel-CodeModel-Methods',
	#package : 'Kernel-CodeModel',
	#tag : 'Methods'
}

{ #category : 'installing' }
OCAnnouncingMethodInstaller >> addAndClassifyMethod: compiledMethod into: behavior [

	<reflection: 'Class structural modification - Selector/Method modification'>
	| oldProtocol newProtocol isNewMethod selector priorOrigin |
	selector := compiledMethod selector.
	isNewMethod := (behavior includesLocalSelector: selector) not.
	behavior compiledMethodAt: selector ifPresent: [ :method |
			priorMethod := method.
			priorOrigin := method origin ].

	super addAndClassifyMethod: compiledMethod into: behavior.
	
	"Protocols are set even for anonymous classes"

	oldProtocol := behavior protocolOfSelector: selector.
	(self codeChangeAnnouncerFor: behavior) prevent: MethodRecategorized during: [ behavior classify: selector under: protocol ]. "If anonymous, do not announce any change."
	behavior isAnonymous ifTrue: [ ^ self ].

	newProtocol := behavior protocolOfSelector: selector.
	
	"The old protocol should never be nil. But we are not in an ideal world sadly :( We got reported some cases where some code was loaded but had an error during the loading preventing a method that got added to the method dictionary to have an associated protocol. If this happens, we consider that we categorize it for the first time to avoid announcing a MethodRecategorized with an old protocol that is nil."
	"The old protocol should never be nil. But we are not in an ideal world sadly :( We got reported some cases where some code was loaded but had an error during the loading preventing a method that got added to the method dictionary to have an associated protocol. If this happens, we consider that we categorize it for the first time to avoid announcing a MethodRecategorized with an old protocol that is nil."
	(priorMethod isNil or: [ priorOrigin ~= compiledMethod origin or: [ oldProtocol isNil ] ])
		ifTrue: [ "If we end up in this branch, it means the method is compiled for the first time in this class. BUT we have an edge case that is that during a class recompilation, we create a new instance of the class and add the methods from the old one to the new one. In that case, we should not add the new method to the package of the newProtocol because it is already present.
			In order to know if we are in this edge case we can check if we have an old protocol already because we copy the protocols before the methods, but only in case the prior origin is the same as the origin. Else, it means that we are overrinding a method from a trait.."
				((priorMethod isNotNil and: [ priorMethod isFromTrait ]) or: [ oldProtocol isNil ]) ifTrue: [
					(behavior packageOrganizer packageForProtocol: newProtocol from: behavior) addMethod: compiledMethod ].
				behavior codeChangeAnnouncer announce: (MethodAdded method: compiledMethod) ]
		ifFalse: [ "If protocol changed and someone is from different package, I need to throw a method recategorized"
				newProtocol = oldProtocol ifFalse: [ behavior announceRecategorizationOf: compiledMethod oldProtocol: oldProtocol ].
				behavior codeChangeAnnouncer announce: (MethodModified methodChangedFrom: priorMethod to: compiledMethod oldProtocol: oldProtocol) ]
]

{ #category : 'installing' }
OCAnnouncingMethodInstaller >> codeChangeAnnouncer: anAnnouncer [

	codeChangeAnnouncer := anAnnouncer
]

{ #category : 'installing' }
OCAnnouncingMethodInstaller >> codeChangeAnnouncerFor: behavior [

	^ codeChangeAnnouncer ifNil: [ behavior codeChangeAnnouncer ]
]
