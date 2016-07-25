pages
changeLog
	^ HelpTopic 
		title: 'ChangeLog'
		contents: 'New Stuff in Pharo 5.0
----------------------

- The PharoVM is now based on Spur, the new memory management, and it brings with it a 35% speedup!
- A new unified foreign function interface (UFFI) replaced NativeBoost to provide a strong Spur-compatible framework for interfacing with the outside world.
- The Glamorous Toolkit now includes the GTDebugger to offer a moldable infrastructure that allows the developer to customize the debugger deeply.
- The underlying Reflectivity mechanism has reached maturity with multiple pieces coming together to empower developers to instrument their own systems. For example, we now have breakpoints implemented as just a simple extension of this mechanism.
- QualityAssistant is now part of the image to provide live feedback during development.

All Issues
----------
Many changes have been made to support the generation of a more modular system. This release integrates  a quite large number of fixes and enhancements: more than 2450! 
List is so big that we cannot put it here  (it would have any sense), but you can review all 
issues at FogBugz issue tracker (https://pharo.fogbugz.com) (you will need an account there, sorry).

You can see the Pharo 5.0 changelog at: 

https://github.com/pharo-project/pharo-changelogs/blob/master/Pharo50ChangeLogs.md
'