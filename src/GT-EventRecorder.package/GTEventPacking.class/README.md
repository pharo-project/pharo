I represent an abstract packing system that is able to receive objects and pack them into a binary data (ByteArray).

Others can ask me for serializing an object by sending me #pack: or for materializing a ByteArray by sending me #unpack:. On the class side, you can find out #fuel and #ston methods, two available packing strategies. The method #version keeps current packing version and changelog. 

The main users are GTEventRecorder (for packing) and GTEventAnnouncement (for unpacking).

Public API and Key Messages

- pack:
- unpack:
- version
