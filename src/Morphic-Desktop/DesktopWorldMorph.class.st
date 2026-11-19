"
I am the world of the Pharo desktop. For now I only exist to keep what a desktop does apart from Morphic's core world; the desktop behaviour itself is in root event handlers (DesktopRootEventHandler).

Morphic's initialization installs me: DesktopWorldMorph installNewWorld.
"
Class {
	#name : 'DesktopWorldMorph',
	#superclass : 'WorldMorph',
	#category : 'Morphic-Desktop',
	#package : 'Morphic-Desktop'
}

{ #category : 'notifying' }
DesktopWorldMorph >> notifyUser: aStringOrText [
	"Tell the user aStringOrText, with a notification that fades away"

	GrowlMorph
		openWithLabel: 'Information' translated
		contents: aStringOrText
]
