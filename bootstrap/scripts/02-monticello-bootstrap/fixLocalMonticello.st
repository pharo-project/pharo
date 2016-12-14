RPackageOrganizer default unregisterPackageNamed: 'Compression-Archives'.
RPackageOrganizer default unregisterPackageNamed: 'Compression-Streams'.!


!RPackageOrganizer methodsFor: 'private registration' stamp: 'MarcusDenker 7/16/2014 17:25'!
basicUnregisterPackage: aPackage
	"Unregister the specified package from the list of registered packages. Raise the announcement RPackageUnregistered. This is a low level action. It does not unregister the back pointer from classes to packages or any other information managed by the organizer"
	
	packages
		removeKey: aPackage name asSymbol
		ifAbsent: [ self reportExtraRemovalOf: aPackage ].
	"unregister also mc package"
	aPackage mcPackage 
		ifNotNil: [ :mcPackage | mcPackage workingCopy unregister ].! !




'From Pharo5.0 of 16 April 2015 [Latest update: #50752] on 10 May 2016 at 1:36:00.128099 pm'!

!RPackageOrganizer methodsFor: 'private registration' stamp: 'EstebanLorenzano 2/21/2014 12:56'!
basicRegisterPackage: aPackage
	"A new package is now available and declared in the receiver. Note that it is a low level implementation method since it does not deal with package contained information and does not update the related mapping tables."
	
	packages 
		at: aPackage name asSymbol 
		put: aPackage.
	"register mc package"
	MCWorkingCopy forPackage: (MCPackage named: aPackage name).
	
	^ aPackage! !

!


'From Pharo6.0 of 13 May 2016 [Latest update: #60290] on 14 November 2016 at 5:38:22.432521 pm'!


'From Pharo5.0 of 16 April 2015 [Latest update: #50752] on 10 May 2016 at 3:35:27.377099 pm'!

!RPackageOrganizer class methodsFor: 'singleton' stamp: 'StephaneDucasse 6/3/2012 22:39'!
resetDefault

	"self resetDefault"
	default  ifNotNil: [
		"When an Organizer is not used anymore, we should always pay attention that these two actions are corretly done."
		default unregisterInterestToSystemAnnouncement.
		MCWorkingCopy removeDependent: default].
	default := self new.
	self packageClass initialize! !


'From Pharo5.0 of 16 April 2015 [Latest update: #50752] on 10 May 2016 at 3:35:38.129822 pm'!

!RPackageOrganizer methodsFor: 'private' stamp: 'CamilleTeruel 7/29/2012 18:47'!
stopNotification
	"(self instVarNamed: #default) stopNotification"
	
	"pay attention that we can break the system using this method"
	
	SystemAnnouncer uniqueInstance unsubscribe: self.
	MCWorkingCopy removeDependent: self.! !
	

'From Pharo5.0 of 16 April 2015 [Latest update: #50752] on 10 May 2016 at 3:36:09.623307 pm'!

!RPackageOrganizer methodsFor: 'system integration' stamp: 'BernardoContreras 10/24/2014 17:36'!
registerInterestToAnnouncer: anAnnouncer	
	"There should be only one"
	anAnnouncer unsubscribe: self.
	
	anAnnouncer weak
		when: CategoryAdded send: #systemCategoryAddedActionFrom: to: self;
		when: CategoryRemoved send: #systemCategoryRemovedActionFrom: to: self;
		when: CategoryRenamed send: #systemCategoryRenamedActionFrom: to: self;
		when:  ClassAdded send: #systemClassAddedActionFrom: to: self;
		when:  ClassRecategorized send: #systemClassRecategorizedActionFrom: to: self;
		when:  ClassRemoved send: #systemClassRemovedActionFrom: to: self;
		when:  ClassRenamed send: #systemClassRenamedActionFrom: to: self;
		when:  ClassReorganized send: #systemClassReorganizedActionFrom: to: self;
		when:  MethodAdded send: #systemMethodAddedActionFrom: to: self;
		when:  MethodModified send: #systemMethodModifiedActionFrom: to: self;
		when:  MethodRecategorized send: #systemMethodRecategorizedActionFrom: to: self;
		when:  MethodRemoved send: #systemMethodRemovedActionFrom: to: self;
		when:  ProtocolAdded send: #systemProtocolAddedActionFrom: to: self;
		when:  ProtocolRemoved send: #systemProtocolRemovedActionFrom: to: self.
		
	self flag: #hack. "for decoupling MC"
	self class environment at: #MCWorkingCopy ifPresent: [ 
		anAnnouncer weak
			when: (Smalltalk globals at: #MCWorkingCopyCreated) 
				send: #updateAfterNewMCPackageRegistred: 
				to: self;
			when: (Smalltalk globals at: #MCWorkingCopyDeleted) 
				send: #updateAfterNewMCPackageUnregistred: 
				to: self	].! !

			
RPackageOrganizer default registerInterestToAnnouncer: SystemAnnouncer uniqueInstance.

(RPackageOrganizer default packageNames) asSortedCollection do: [:packageName |
  Transcript show: 'snapshot of: '; show: packageName; cr.
  (MCWorkingCopy forPackage: (MCPackage named: packageName)) snapshot ].
  