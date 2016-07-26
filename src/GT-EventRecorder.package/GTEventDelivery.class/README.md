I know how to deliver usage and diagnostic data to a remote server.

I start and maintain a process for sending data on a regular inteval (#standardDuration).
I maintain a queue with the data that needs to be send. If a delivert fails I re-add the data to the queue.

Data that is added to my queue must be packaged in an object of type GTEventBundle.  I use the url specified by the bundle.
I send the recorder the message #pack to notify it that I enter the delivery cycle.

Public API and Key Messages

- #activate initializes the delivery process; clients must call this method; it's not called automatically on object creation. 
- #deliveryCycle implements  the main logic of the delivery process