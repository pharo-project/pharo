I represent a service

provider : the service provider
label : to be display in a menu
selector : to do the service
useLineAfter
stateSelector : a secondary selector (to be able to query state of the provider for example)
description : a description for balloon for example
argumentGetter : a selector to get additional arguments with (if selector requres them)
buttonLabel : a short label

The entire client interface (provided by FileList and other users of the registry)
is this (browse #getArgumentsFrom: and the 
senders of #argumentGetter:):

fullName (returns a String with the full filename)
dirAndFileName (returns {directory. fileName})
readOnlyStream (returns an open read-only stream)
