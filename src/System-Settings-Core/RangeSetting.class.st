"
I represent a setting for which the domain value is an interval.
"
Class {
	#name : 'RangeSetting',
	#superclass : 'Setting',
	#instVars : [
		'range'
	],
	#category : 'System-Settings-Core-Base-New',
	#package : 'System-Settings-Core',
	#tag : 'Base-New'
}

{ #category : 'converting' }
RangeSetting >> asSettingPresenterWithOwner: aPresenter [
	"Answer an appropriate <SpPresenter> container for the receiver's value"

	^ aPresenter instantiate: StSettingDropListPresenterItem on: self
]

{ #category : 'accessing' }
RangeSetting >> domainValues [

	^ self range collect: [ :each | MultipleSettingItem newFromObject: each setting: self ]
]

{ #category : 'accessing' }
RangeSetting >> range [
	^ range
]

{ #category : 'accessing' }
RangeSetting >> range: anInterval [ 
	range := anInterval
]
