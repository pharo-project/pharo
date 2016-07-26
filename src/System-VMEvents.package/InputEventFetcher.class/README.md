EventFetcher is responsible for fetching the raw VM events and forward them to the registered event handlers. Event fetching is done in a high priority process, so even with other processes (e.g. the Morphic UI process) being busy events will still be fetched.

Instance Variables
	inputSemaphore:		<Semaphore>
	eventHandlers		<OrderedCollection>
	fetcherProcess		<Process>

inputSemaphore
	- a semaphore registered with the VM to signal availability of an event. Currently not supported on all platforms.

eventHandlers
	- registered event handlers. Event buffers are cloned before sent to each handler.

fetcherProcess
	- a process that fetches the events from the VM. Either polling (InputEventPollingFetcher) or waiting on the inputSemaphore.



Event format:
The current event format is very simple. Each event is recorded into an 8 element array. All events must provide some SmallInteger ID (the first field in the event buffer) and a time stamp (the second field in the event buffer), so that the difference between the time stamp of an event and the current time can be reported.

Currently, the following events are defined:

Null event
=============
The Null event is returned when the ST side asks for more events but no more events are available.
Structure:
[1]		- event type 0
[2-8]	- unused

Mouse event structure
==========================
Mouse events are generated when mouse input is detected.
Structure:
[1]	- event type 1
[2]	- time stamp
[3]	- mouse x position
[4]	- mouse y position
[5]	- button state; bitfield with the following entries:
		1	-	yellow (e.g., right) button
		2	-	blue (e.g., middle) button
		4	-	red (e.g., left) button
		[all other bits are currently undefined]
[6]	- modifier keys; bitfield with the following entries:
		1	-	shift key
		2	-	ctrl key
		4	-	(Mac specific) option key
		8	-	Cmd/Alt key
		[all other bits are currently undefined]
[7]	- reserved.
[8]	- reserved.

Keyboard events
====================
Keyboard events are generated when keyboard input is detected.
[1]	- event type 2
[2]	- time stamp
[3]	- character code
		For now the character code is in Mac Roman encoding.
[4]	- press state; integer with the following meaning
		0	-	character
		1	-	key press (down)
		2	- 	key release (up)
[5]	- modifier keys (same as in mouse events)
[6]	- reserved.
[7]	- reserved.
[8]	- reserved.
