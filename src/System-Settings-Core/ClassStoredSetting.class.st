"
I am responsible for keeping class name.
"
Class {
	#name : 'ClassStoredSetting',
	#superclass : 'StoredSetting',
	#category : 'System-Settings-Core-Persistence-Ston-StoredSettings',
	#package : 'System-Settings-Core',
	#tag : 'Persistence-Ston-StoredSettings'
}

{ #category : 'accessing' }
ClassStoredSetting >> realValue [
	^ self class environment at: realValue
]
