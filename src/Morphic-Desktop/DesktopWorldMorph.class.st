"
I am the world of the Pharo desktop: a world with the desktop's root event handlers (world menu and quit dialog, taskbar keys, halos), and the desktop's way to notify the user (a Growl).

Morphic's initialization installs me as the world: DesktopWorldMorph installNewWorld. I can also be created on my own, or embedded in another morph: I am the root of what I hold.
"
Class {
	#name : 'DesktopWorldMorph',
	#superclass : 'WorldMorph',
	#category : 'Morphic-Desktop',
	#package : 'Morphic-Desktop'
}

{ #category : 'initialization' }
DesktopWorldMorph >> initialize [
	"A desktop: its world menu and quit dialog, its taskbar keys, and halos on meta gestures"

	super initialize.
	self
		addRootEventHandler: DesktopRootEventHandler new;
		addRootEventHandler: TaskbarRootEventHandler new;
		addRootEventHandler: HaloRootEventHandler new
]

{ #category : 'notifying' }
DesktopWorldMorph >> notifyUser: aStringOrText [
	"Tell the user aStringOrText, with a notification that fades away"

	(GrowlMorph label: 'Information' translated contents: aStringOrText) openInWorld: self
]
