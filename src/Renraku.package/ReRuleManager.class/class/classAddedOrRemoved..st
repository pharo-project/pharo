event subscriptions
classAddedOrRemoved: aClassAddedAnnouncement 
	| class |
	class := aClassAddedAnnouncement classAffected.

	((class inheritsFrom: RBLintRule) or: [ 
	  class inheritsFrom: ReAbstractRule ]) 
		ifTrue: [ self reset ] 