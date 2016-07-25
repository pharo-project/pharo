This class implements TCP/IP style network name lookup and translation facilities.

Attempt to keep track of whether there is a network available.
HaveNetwork	true if last attempt to contact the network was successful.
LastContact		Time of that contact (totalSeconds).
haveNetwork	returns true, false, or #expired.  True means there was contact in the last 30 minutes.  False means contact failed or was false last time we asked.  Get out of false state by making contact with a server in some way (FileList or updates).