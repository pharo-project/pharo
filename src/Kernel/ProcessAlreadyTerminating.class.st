"
I notify the sender of #terminate that the receiving process has already received a #terminate before.

Use Process>>isTerminating to check for this.
"
Class {
	#name : 'ProcessAlreadyTerminating',
	#superclass : 'Notification',
	#category : 'Kernel-Processes',
	#package : 'Kernel',
	#tag : 'Processes'
}
