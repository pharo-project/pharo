Change Set:		ProcessBrowser
Date:			14 March 2000
Author:			Ned Konz

email: ned@bike-nomad.com

This is distributed under the Squeak License.

Added 14 March:
	CPUWatcher integration
	automatically start and stop CPUWatcher
	added CPUWatcher to process list menu

Added 29 October:
	MVC version
	2.8, 2.7 compatibility
	rearranged menus
	added pointer inspection and chasing
	added suspend/resume
	recognized more well-known processes
	misc. bug fixes

Added 26 October: highlight pc in source code
Added 27 October: added 'signal semaphore'
added 'inspect receiver', 'explore receiver', 'message tally' to stack list menu
added 'find context', 'next context' to process list menu
added 'change priority' and 'debug' choices to process list menu

27 October mods by Bob Arning:

alters process display in Ned's ProcessBrowser to 
- show process priority
- drop 'a Process in' that appears on each line
- show in priority order
- prettier names for known processes
- fix to Utilities to forget update downloading process when it ends (1 less dead
process)
- correct stack dump for the active process
