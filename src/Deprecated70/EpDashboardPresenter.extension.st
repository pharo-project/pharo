Extension { #name : #EpDashboardPresenter }

{ #category : #'*Deprecated70' }
EpDashboardPresenter >> createLogButtonModel [
	self deprecated: 'Use #createLogButtonPresenter instead' transformWith: '`@receiver createLogButtonModel' -> '`@receiver createLogButtonPresenter'.
	
	^ self createLogButtonPresenter
]

{ #category : #'*Deprecated70' }
EpDashboardPresenter >> currentSessionInfoModel [
	self deprecated: 'Use #currentSessionInfoPresenter instead' transformWith: '`@receiver currentSessionInfoModel' -> '`@receiver currentSessionInfoPresenter'.
	
	^ self currentSessionInfoPresenter
]

{ #category : #'*Deprecated70' }
EpDashboardPresenter >> enableMonitorButtonModel [
	self deprecated: 'Use #enableMonitorButtonPresenter instead' transformWith: '`@receiver enableMonitorButtonModel' -> '`@receiver enableMonitorButtonPresenter'.
	
	^ self enableMonitorButtonPresenter
]

{ #category : #'*Deprecated70' }
EpDashboardPresenter >> initializeCreateLogButtonModel [
	self deprecated: 'Use #initializeCreateLogButtonPresenter instead' transformWith: '`@receiver initializeCreateLogButtonModel' -> '`@receiver initializeCreateLogButtonPresenter'.
	
	^ self initializeCreateLogButtonPresenter
]

{ #category : #'*Deprecated70' }
EpDashboardPresenter >> initializeEnableMonitorButtonModel [
	self deprecated: 'Use #initializeEnableMonitorButtonPresenter instead' transformWith: '`@receiver initializeEnableMonitorButtonModel' -> '`@receiver initializeEnableMonitorButtonPresenter'.

	^ self initializeEnableMonitorButtonPresenter

]

{ #category : #'*Deprecated70' }
EpDashboardPresenter >> initializeOpenSettingsButtonModel [
	self deprecated: 'Use #openSettingsButtonPresenter instead' transformWith: '`@receiver initializeOpenSettingsButtonModel' -> '`@receiver openSettingsButtonPresenter'.

	^ self initializeOpenSettingsButtonPresenter
]

{ #category : #'*Deprecated70' }
EpDashboardPresenter >> openSettingsButtonModel [
	self deprecated: 'Use #openSettingsButtonPresenter instead' transformWith: '`@receiver openSettingsButtonModel' -> '`@receiver openSettingsButtonPresenter'.

	^ self openSettingsButtonPresenter
]

{ #category : #'*Deprecated70' }
EpDashboardPresenter >> refreshCurrentSessionInfoModel [

	self deprecated: 'Use #refreshCurrentSessionInfoPresenter instead' transformWith: '`@receiver refreshCurrentSessionInfoModel' -> '`@receiver refreshCurrentSessionInfoPresenter'.
	
	^ self refreshCurrentSessionInfoPresenter
]

{ #category : #'*Deprecated70' }
EpDashboardPresenter >> refreshEnableMonitorButtonModel [
	self deprecated: 'Use #refreshEnableMonitorButtonPresenter instead' transformWith: '`@receiver refreshEnableMonitorButtonModel' -> '`@receiver refreshEnableMonitorButtonPresenter'.
		
	^ self refreshEnableMonitorButtonPresenter
]
