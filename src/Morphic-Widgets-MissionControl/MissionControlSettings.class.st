Class {
	#name : 'MissionControlSettings',
	#superclass : 'Object',
	#classVars : [
		'AnimateTransitions',
		'OverlayOpacity',
		'ShowWindowLabels',
		'ThumbnailPadding'
	],
	#category : 'Morphic-Widgets-MissionControl',
	#package : 'Morphic-Widgets-MissionControl'
}

{ #category : 'settings' }
MissionControlSettings class >> animateTransitions [

	^ AnimateTransitions ifNil: [ AnimateTransitions := false ]
]

{ #category : 'settings' }
MissionControlSettings class >> animateTransitions: aBoolean [

	AnimateTransitions := aBoolean
]

{ #category : 'settings' }
MissionControlSettings class >> missionControlSettingsOn: aBuilder [

	<systemsettings>
	(aBuilder group: #missionControl)
		parent: #morphic;
		label: 'Mission Control';
		description: 'Settings for the Mission Control window overview feature.';
		with: [
				(aBuilder setting: #overlayOpacity)
					target: self;
					label: 'Overlay opacity';
					default: 0.5;
					description: 'Opacity of the dark overlay shown behind window thumbnails (0.0 to 1.0).'.
				(aBuilder setting: #thumbnailPadding)
					target: self;
					label: 'Thumbnail padding';
					default: 20;
					description: 'Padding in pixels between window thumbnails.'.
				(aBuilder setting: #showWindowLabels)
					target: self;
					label: 'Show window labels';
					default: true;
					description: 'Whether to show window titles below thumbnails.'.
				(aBuilder setting: #animateTransitions)
					target: self;
					label: 'Animate transitions';
					default: false;
					description: 'Whether to animate entering and exiting Mission Control.'
		]
]

{ #category : 'settings' }
MissionControlSettings class >> overlayOpacity [

	^ OverlayOpacity ifNil: [ OverlayOpacity := 0.5 ]
]

{ #category : 'settings' }
MissionControlSettings class >> overlayOpacity: opacity [

	OverlayOpacity := opacity
]

{ #category : 'settings' }
MissionControlSettings class >> showWindowLabels [

	^ ShowWindowLabels ifNil: [ ShowWindowLabels := true ]
]

{ #category : 'settings' }
MissionControlSettings class >> showWindowLabels: aBoolean [

	ShowWindowLabels := aBoolean
]

{ #category : 'settings' }
MissionControlSettings class >> thumbnailPadding [

	^ ThumbnailPadding ifNil: [ ThumbnailPadding := 20 ]
]

{ #category : 'settings' }
MissionControlSettings class >> thumbnailPadding: anInteger [

	ThumbnailPadding := anInteger
]
