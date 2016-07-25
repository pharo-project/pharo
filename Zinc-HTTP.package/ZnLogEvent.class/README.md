I am ZnLogEvent, the base class of a log events emitted by elements of the Zinc HTTP Components framework.

I add a timestamp and a simple id attribute. The id can wrap around and should only be used to distinguish between events that have the same timestamp.

ZnLogEvents are distributed as Announcement through a singleton Announcer that I maintain.

I have a small convenience API to log to the Transcript or open a simple GUI on the emitted log events.