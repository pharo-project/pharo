I define the API of Transcript, which is quite similar to stream.

A Transcript acts like a character write stream, accumulating output send to it. Some operations buffer output without necessarily showing it (like #nextPut: nextPutAll: #print: #cr #space and #tab), while others add their output and show any buffered output (like #show: #crShow: and <<). Use #flush or #endEntry to force buffered output to be shown.

Clients are required to implement #critical: #flush: #nextPut: and #nextPutAll: 

Note that #critical: should serialize multithreaded access

Client can optionally implement #close or #clear themselves

Historical note: #ensureCr and #reset were removed since they were not used