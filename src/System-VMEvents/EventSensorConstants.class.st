"
This is a constants, used by EventSensor to identify various event types and their contents.

NOTE: A constant values should be kept in sync with VM-side, which are defined in sq.h header file. 
"
Class {
	#name : #EventSensorConstants,
	#superclass : #SharedPool,
	#classVars : [
		'BlueButtonBit',
		'ButtonsBitMask',
		'CommandKeyBit',
		'CtrlKeyBit',
		'EventKeyChar',
		'EventKeyDown',
		'EventKeyUp',
		'EventTypeDragDropFiles',
		'EventTypeIndexInBuffer',
		'EventTypeKeyboard',
		'EventTypeMenu',
		'EventTypeMouse',
		'EventTypeNone',
		'EventTypeWindow',
		'KeyboardModifierIndexInEventBuffer',
		'ModifierKeysBitMask',
		'MouseButtonIndexInEventBuffer',
		'MouseModifierIndexInEventBuffer',
		'MouseXCoordinateIndexInEventBuffer',
		'MouseYCoordinateIndexInEventBuffer',
		'OptionKeyBit',
		'RedButtonBit',
		'ShiftKeyBit',
		'WindowEventActivated',
		'WindowEventClose',
		'WindowEventIconise',
		'WindowEventMetricChange',
		'WindowEventPaint',
		'YellowButtonBit'
	],
	#category : #'System-VMEvents-Support'
}

{ #category : #'class initialization' }
EventSensorConstants class >> initialize [

	self 
		initializeButtonBits;
		initializeSpecialKeyBits;
		initializeEventTypeConstants;
		initializeEventKeyConstants;
		initializeWindowEventConstants;
		initializeEventIndexesConstants
]

{ #category : #'private - initialization' }
EventSensorConstants class >> initializeButtonBits [

	RedButtonBit := 4.
	BlueButtonBit := 1.
	YellowButtonBit := 2.
	ButtonsBitMask := RedButtonBit | BlueButtonBit | YellowButtonBit
]

{ #category : #'private - initialization' }
EventSensorConstants class >> initializeEventIndexesConstants [
	EventTypeIndexInBuffer := 1.
	MouseModifierIndexInEventBuffer := 6.
	MouseButtonIndexInEventBuffer := 5.
	MouseXCoordinateIndexInEventBuffer := 3.
	MouseYCoordinateIndexInEventBuffer := 4.
	KeyboardModifierIndexInEventBuffer := 5
]

{ #category : #'private - initialization' }
EventSensorConstants class >> initializeEventKeyConstants [
	"Press codes for keyboard events"
	
	EventKeyChar := 0.
	EventKeyDown := 1.
	EventKeyUp := 2.
]

{ #category : #'private - initialization' }
EventSensorConstants class >> initializeEventTypeConstants [
	"Types of events"
	
	EventTypeNone := 0.
	EventTypeMouse := 1.
	EventTypeKeyboard := 2.
	EventTypeDragDropFiles := 3.
	EventTypeMenu := 4.
	EventTypeWindow := 5
]

{ #category : #'private - initialization' }
EventSensorConstants class >> initializeSpecialKeyBits [

	ShiftKeyBit := 1.
	CtrlKeyBit := 2.
	OptionKeyBit := 4.
	CommandKeyBit := 8.
	ModifierKeysBitMask := OptionKeyBit | CommandKeyBit | CtrlKeyBit
]

{ #category : #'private - initialization' }
EventSensorConstants class >> initializeWindowEventConstants [
	"Window event action codes"

	WindowEventMetricChange := 1. "Size or position of window changed - value1-4 are left/top/right/bottom values"
	WindowEventClose := 2. "Window close icon pressed"
	WindowEventIconise := 3. "Window iconised or hidden etc."
	WindowEventActivated :=4. "Window made active - some platforms only - do not rely upon this"
	WindowEventPaint := 5. "Window area (in value1-4) needs updating. Some platforms do not need to send this, do not rely on it in image"	
]
