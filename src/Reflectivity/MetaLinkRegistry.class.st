"
I register specific kind of links that are instance specific and/or ""permanent"".

I do not install links, i only know informations necessary to find helpful information for link installation (target instances, link count, nodes...).

### Instance specific links

These links are installed for a given object. 

links <WeakKeyDictionary> I reference target objects (values) for which a link has been installed on (key)

Note that instance specific links can be put on other nodes for other objects and/or classes. An instance specific link can also be a regular link active for all instances of an other classes, or even be put on an other node in the class of the object.

### PermaLinks

These links are installed on slots, temporary variables or class variables. I hold explicit specifications of all the kind of nodes the link should permanently be installed on.

Specifications can be of kind for a given var: 
- #read install on all read nodes of var
- #write install on all assignment nodes of var 
- #all both read and write nodes of var

Usecases:

- A link is put on all reads of slot s of class C. If a new method reading s is added in C, i must hold necessary informations to find the new read nodes of s in the new method. I neither find the nodes nor install links on them myself.

- A link is put on all assignments of slot s of class C. If a method with an assignement of s is modified in C, in must hold necessary informations to check if there is still any assignements nodes of s in this method, and update the link accordingly (i.e. reinstall it on these new nodes if they do exist)

In both these usecases, i provide fast and convenient access to the needed informations.
"
Class {
	#name : #MetaLinkRegistry,
	#superclass : #Object,
	#instVars : [
		'links',
		'permalinks',
		'classes',
		'permanentTargets'
	],
	#category : #'Reflectivity-Installer'
}

{ #category : #adding }
MetaLinkRegistry >> addMetaLink: aLink forObject: anObject [
	(links at: aLink ifAbsentPut: WeakSet new) add: anObject
]

{ #category : #permalinks }
MetaLinkRegistry >> canReinstallPermaLink: permalink [
	^ (self hasPermaLinks: permalink link) not
		or: [ | slotsOrVarsWithPermalink |
			slotsOrVarsWithPermalink := classes at: permalink slotOrVarClass ifAbsent: [ #() ].
			slotsOrVarsWithPermalink noneSatisfy: [ :slotOrVar | permalink slotOrVariable == slotOrVar ] ]
]

{ #category : #'registry access' }
MetaLinkRegistry >> countObjectSpecificLinksIn: someLinks [
	someLinks ifNil:[^0].
	^ (someLinks select: [ :link | self isLinkObjectSpecific: link ]) size
]

{ #category : #permalinks }
MetaLinkRegistry >> hasPermaLinks: aLink [
	^ permalinks keys includes: aLink
]

{ #category : #initialize }
MetaLinkRegistry >> initialize [
	links := WeakKeyDictionary new.
	permalinks := WeakKeyDictionary new.
	classes := WeakKeyDictionary new.
	permanentTargets := WeakKeyDictionary new
]

{ #category : #'registry access' }
MetaLinkRegistry >> isLinkObjectSpecific: aLink [
	^ links keys includes: aLink
]

{ #category : #'registry access' }
MetaLinkRegistry >> isNodeWithInstanceSpecificLinks: node [
	^ (self countObjectSpecificLinksIn: node links) > 0
]

{ #category : #'registry access' }
MetaLinkRegistry >> objectsForLink: aMetaLink [
	^ links at: aMetaLink ifAbsent: [ Array new ]
]

{ #category : #permalinks }
MetaLinkRegistry >> permaLinksFor: aLink [
	^ permalinks at: aLink ifAbsent: [ #() ]
]

{ #category : #permalinks }
MetaLinkRegistry >> permaLinksForClasses: targetClasses [
	| allPermalinks |
	allPermalinks := permalinks values flattened.
	^ allPermalinks select: [ :p | (targetClasses select: [ :c | p notNil and: [ c == p slotOrVarClass ] ]) notEmpty ]
]

{ #category : #permalinks }
MetaLinkRegistry >> permaLinksForMethod: method [
	| class targets |
	class := method methodClass.
	targets := classes at: class ifAbsent: [ #() ].
	^ (permanentTargets associationsSelect: [ :assoc | targets includes: assoc key ]) values
		flattened select: [ :permalink | permalink slotOrVarClass == class ]
]

{ #category : #permalinks }
MetaLinkRegistry >> registerPermaLink: aPermaLink [
	| registeredTargets registeredLinks registeredSlots |
	registeredTargets := permanentTargets at: aPermaLink slotOrVariable ifAbsentPut: [ Set new ].
	registeredTargets add: aPermaLink.
	
	registeredLinks := permalinks at: aPermaLink link ifAbsentPut: [ Set new ].
	registeredLinks add: aPermaLink.
	
	registeredSlots := classes at: aPermaLink slotOrVarClass ifAbsentPut: [ Set new ].
	registeredSlots add: aPermaLink slotOrVariable
]

{ #category : #permalinks }
MetaLinkRegistry >> unregisterPermaLinksFor: aLink [
	| permaLinks |
	permaLinks := (self permaLinksFor: aLink) ifEmpty: [ ^ self ].
	permaLinks do: [ :permaLink | (permanentTargets at: permaLink slotOrVariable) remove: permaLink ].
	permalinks removeKey: aLink
]
