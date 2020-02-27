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
		'createLogButton',
		'enableMonitorButton',
		'openSettingsButton',
		'currentSessionInfoPresenter'
	],
	#category : #'EpiceaBrowsers-UI-Dashboard'
}

{ #category : #specs }
EpDashboardPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #currentSessionInfoPresenter;
		add:
			(SpBoxLayout newHorizontal
					add: #enableMonitorButton;
					add: #createLogButton;
					add: #openSettingsButton;
					yourself)
			expand: false
			fill: false
			padding: 0;
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

{ #category : #initialization }
EpDashboardPresenter >> initializeCreateLogButton [
	createLogButton
		icon: (self iconNamed: #addIcon);
		help: 'Start recording into new Log';
		label: 'New session';
		action: [ self createNewSessionLog ]
]

{ #category : #initialization }
EpDashboardPresenter >> initializeEnableMonitorButton [
	enableMonitorButton
		help: 'Enable/disable the Epicea Monitor. When enabled, it logs code changes (and some IDE events such as MC load/save and refactorings) performed in this Pharo session.';
		action: [ self monitor enabled: enableMonitorButton state not ]
]

{ #category : #initialization }
EpDashboardPresenter >> initializeOpenSettingsButton [
	openSettingsButton
		help: 'Open Epicea''s system settings';
		icon: (self iconNamed: #config);
		label: 'More settings...';
		action: [ self openSettingsBrowser ]
]

{ #category : #initialization }
EpDashboardPresenter >> initializePresenter [ 
	super initializePresenter.

	"Listen announcers"
	self initializeRefreshEnableMonitorButtonModelOnMonitorUpdate.
]

{ #category : #initialization }
EpDashboardPresenter >> initializeRefreshEnableMonitorButtonModelOnMonitorUpdate [
	self monitor announcer weak when: EpMonitorStateUpdated send: #refreshEnableMonitorButtonPresenter to: self
]

{ #category : #initialization }
EpDashboardPresenter >> initializeWidgets [
	currentSessionInfoPresenter := self newText.
	enableMonitorButton := self newButton.
	openSettingsButton := self newButton.
	createLogButton := self newButton.

	self initializeCreateLogButton.
	self initializeEnableMonitorButton.
	self initializeOpenSettingsButton.

	currentSessionInfoPresenter enabled: false
]

{ #category : #initialization }
EpDashboardPresenter >> initializeWindow: aWindowPresenter [
	aWindowPresenter
		title: 'Epicea - Dashboard';
		initialExtent: 450 @ 200;
		whenClosedDo: [ self monitor announcer unsubscribe: self ]
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

{ #category : #refreshing }
EpDashboardPresenter >> refresh [
	self refreshEnableMonitorButtonPresenter.
	self refreshCurrentSessionInfoPresenter
]

{ #category : #refreshing }
EpDashboardPresenter >> refreshCurrentSessionInfoPresenter [
	| sessionStore text |
	sessionStore := self monitor sessionStore.

	text := String
		streamContents: [ :stream | 
			stream
				nextPutAll: 'Current Log: ';
				cr;
				nextPutAll: sessionStore writingFileReference basename;
				cr;
				cr;
				nextPutAll: 'Directory: ';
				cr;
				nextPutAll: sessionStore writingFileReference parent fullName ].

	currentSessionInfoPresenter text: text
]

{ #category : #refreshing }
EpDashboardPresenter >> refreshEnableMonitorButtonPresenter [
	enableMonitorButton
		state: self monitor isEnabled;
		label: self monitor statusString;
		icon: (self iconNamed: self monitor statusIcon)
]
