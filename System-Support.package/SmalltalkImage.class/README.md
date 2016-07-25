My singleton is a central entry point to the system.

It has many roles and responsibilities (to be documented)
	- startup/shutdown image
	- source management
	- namespace access
	- tool access
	- platform access
	- VM information and parameters	
	
Startup
-----------
At startup and shutdown the image execute the methods startUp: and shutdown: of registered classes (registered using addToStartUpList:, addToShutDownList:, ... methods and friends).
	
Startup phases
During the first stage of start up the UI manager, the default uimanager is switched to a specific non interactive ui manager (StartupUIManager). Note that this specific non interactive UIManager kills the system on any attempt to open windows and interaction. So be warned, don't use interaction in the first phase. 
Then all registered classes execute their start up procedures (which should not imply interactive behavior). 
After startup list is finished, any deferred startup actions are executed, which you can add using the method
addDeferredStartupAction: method.  





