"
I am a notification that should display its message text to the user as an information.

This feature should be added by the IDE of Pharo to not have the kernel depending on the UI layer of Pharo. If it is not loaded, it will fallback to the class printing in the stdout.
"
Class {
	#name : 'InformativeNotification',
	#superclass : 'SystemNotification',
	#category : 'Kernel-Exceptions',
	#package : 'Kernel',
	#tag : 'Exceptions'
}
