The Athens canvas is a central object which is used to performs drawings on a surface.
Please note, that Athens does not allows you to instantiate canvas directly, instead you obtain a ready for use instance as an argument in 
#drawDuring: message, sent to athens surface:

surface drawDuring: [:canvas | .... ]

Using canvas outside a #drawDuring: method is highly discouraged. Doing so may lead to unpredicted behavior/data corruption/image crash.