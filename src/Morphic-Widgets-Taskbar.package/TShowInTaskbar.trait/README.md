TShowInTaskbar is a trait to enable any morph to be shown in the taskbar. It implements the required parts so that the taskbar can build a button showing the given morph. 

A morph implementing this trait must implement the methods located under "taskbar-required".

Adding a menu when the taskbarbutton is right clicked can be done by implemenenting  #taskbarButtonMenu: to return a custom menu.

For mouseOver action, #taskbarCanShowThumbnail must be implemented to return true, and the behaviour when the mouse enters and leaves the taskbar button in their respective methods.