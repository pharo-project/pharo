I'm a wrapper for a stream optimized for line-by-line access using #nextLine. My instances can be nested.

I read one line ahead. Reading terminates when the stream ends, or if the limitingBlock evaluated with the line answers true. To skip the delimiting line for further reading use #skipThisLine.

Character-based reading (#next) is permitted, too. Send #updatePosition when switching from line-based reading.

See examples at the class side.

--bf 2/19/1999 12:52