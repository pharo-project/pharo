"
I'm a Spec model to manage the global state of Epicea.

Open with:
self open
"
Class {
	#name : #EpDashboardPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'monitor',
		'createLogButtonPresenter',
		'currentSessionInfoPresenter',
		'enableMonitorButtonPresenter',
		'openSettingsButtonPresenter'
	],
	#category : #'EpiceaBrowsers-UI-Dashboard'
}

{ #category : #specs }
EpDashboardPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #currentSessionInfoPresenter;
		add:
			(SpBoxLayout newHorizontal
				add: #enableMonitorButtonPresenter;
				add: #createLogButtonPresenter;
				add: #openSettingsButtonPresenter;
				yourself)
			withConstraints: [ :constraints | constraints height: self buttonHeight ];
		yourself
]

{ #category : #'instance creation' }
EpDashboardPresenter class >> open [

	^ self new
		refresh;
		openWithSpec;
		yourself
]

{ #category : #icons }
EpDashboardPresenter class >> taskbarIconName [

	^ #config

]

{ #category : #specs }
EpDashboardPresenter class >> title [

	^ 'Epicea - Dashboard'
]

{ #category : #'accessing widgets' }
EpDashboardPresenter >> createLogButtonPresenter [
	^ createLogButtonPresenter
]

{ #category : #actions }
EpDashboardPresenter >> createNewSessionLog [
	
	| fileNameOrNil sessionStore |
	sessionStore := self monitor sessionStore.

	fileNameOrNil := UIManager default
		request: 'Enter a file name (without extension) for the new log.'
		initialAnswer: sessionStore nextStoreName.
	fileNameOrNil ifNil: [ ^ self ].

	(self monitor sessionStore existsStoreNamed: fileNameOrNil)
		ifTrue: [ 
			self inform: 'Invalid name: Already used' ]
		ifFalse: [
			self monitor sessionStore resetWithStoreNamed: fileNameOrNil.
			self refresh ]
]

{ #category : #'accessing widgets' }
EpDashboardPresenter >> currentSessionInfoPresenter [
	^ currentSessionInfoPresenter
]

{ #category : #'accessing widgets' }
EpDashboardPresenter >> enableMonitorButtonPresenter [
	^ enableMonitorButtonPresenter
]

{ #category : #initialization }
EpDashboardPresenter >> initialExtent [ 

	^ 450 @ 200
]

{ #category : #initialization }
EpDashboardPresenter >> initializeCreateLogButtonPresenter [
	
	self createLogButtonPresenter
		icon: (self iconNamed: #addIcon);
		help: 'Start recording into new Log';
		label: 'New session';
		action: [ self createNewSessionLog ].
]

{ #category : #initialization }
EpDashboardPresenter >> initializeEnableMonitorButtonPresenter [

	self enableMonitorButtonPresenter
		help: 'Enable/disable the Epicea Monitor. When enabled, it logs code changes (and some IDE events such as MC load/save and refactorings) performed in this Pharo session.';
		action: [ 
			self monitor enabled: self enableMonitorButtonPresenter state not.
		].

]

{ #category : #initialization }
EpDashboardPresenter >> initializeOpenSettingsButtonPresenter [

	self openSettingsButtonPresenter
		help: 'Open Epicea''s system settings';
		icon: (self iconNamed: #config);
		label: 'More settings...';
		action: [ self openSettingsBrowser ]
]

{ #category : #initialization }
EpDashboardPresenter >> initializePresenter [ 
	super initializePresenter.
	
	self initializeCreateLogButtonPresenter.
	self initializeEnableMonitorButtonPresenter.
	self initializeOpenSettingsButtonPresenter.

	"Listen announcers"
	self initializeRefreshEnableMonitorButtonModelOnMonitorUpdate.
]

{ #category : #initialization }
EpDashboardPresenter >> initializeRefreshEnableMonitorButtonModelOnMonitorUpdate [

	| monitorAnnouncer |
	monitorAnnouncer := self monitor announcer.

	monitorAnnouncer weak
		when: EpMonitorStateUpdated 
		send: #refreshEnableMonitorButtonPresenter
		to: self.

	self whenWindowChanged: [ :windowModel | 
		windowModel whenClosedDo: [ 
			monitorAnnouncer unsubscribe: self ] ].

]

{ #category : #initialization }
EpDashboardPresenter >> initializeWidgets [
	currentSessionInfoPresenter := self newText.
	enableMonitorButtonPresenter := self newButton.
	openSettingsButtonPresenter := self newButton.
	createLogButtonPresenter := self newButton
]

{ #category : #accessing }
EpDashboardPresenter >> monitor [
	
	^ monitor ifNil: [ EpMonitor current ]
]

{ #category : #accessing }
EpDashboardPresenter >> monitor: anEpMonitor [

	monitor := anEpMonitor.
	self refreshEnableMonitorButtonPresenter
]

{ #category : #actions }
EpDashboardPresenter >> openSettingsBrowser [
	"Inspired on Glamour's code"

	| settingsBrowser settingsWindow |
	settingsBrowser := SettingBrowser new 
		changePackageSet: (RPackage organizer packageNamed: 'EpiceaBrowsers') asOrderedCollection;
		yourself.
	settingsWindow := settingsBrowser open.
	settingsWindow position: Display extent - settingsWindow extent // 2.
	settingsBrowser expandAll.
]

{ #category : #'accessing widgets' }
EpDashboardPresenter >> openSettingsButtonPresenter [

	^ openSettingsButtonPresenter
]

{ #category : #refreshing }
EpDashboardPresenter >> refresh [

	self refreshEnableMonitorButtonPresenter.
	self refreshCurrentSessionInfoPresenter
]

{ #category : #refreshing }
EpDashboardPresenter >> refreshCurrentSessionInfoPresenter [

	| sessionStore text |
	sessionStore := self monitor sessionStore.

	text := String streamContents: [:stream |
		stream
			nextPutAll: 'Current Log: '; cr;
			nextPutAll: sessionStore writingFileReference basename;
			cr; cr;
			nextPutAll: 'Directory: '; cr;
			nextPutAll: sessionStore writingFileReference parent fullName.
		].

	self currentSessionInfoPresenter enabled: false; text: text.
]

{ #category : #refreshing }
EpDashboardPresenter >> refreshEnableMonitorButtonPresenter [
		
	| theState theIconSelector |
	self monitor isEnabled 
		ifTrue: [ 
			theState := 'enabled'.
			theIconSelector := #testGreen ] 
		ifFalse: [ 
			theState := 'disabled'.
			theIconSelector := #testNotRun ].
	
	self enableMonitorButtonPresenter
		state: self monitor isEnabled;
		label: 'Monitor ', theState;
		icon: (self iconNamed: theIconSelector).

]
