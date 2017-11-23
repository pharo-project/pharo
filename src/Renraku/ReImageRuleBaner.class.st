"
A pluggable functionality used to ban rules in the image scale
"
Class {
	#name : #ReImageRuleBaner,
	#superclass : #Object,
	#category : #'Renraku-Utility'
}

{ #category : #actions }
ReImageRuleBaner class >> ban: aCritique [
	
	ReSystemAnnouncer uniqueInstance
		notifyCritique: aCritique
		of: aCritique sourceAnchor entity
		bannedOn: self.

	(aCritique rule isKindOf: ReAbstractRule)
		ifTrue: [
			aCritique rule class enabled: false ]
]

{ #category : #properties }
ReImageRuleBaner class >> reBanName [
	^ 'The whole image'
]
