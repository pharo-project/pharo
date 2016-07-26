This kind of WeakRegistry using a new VM feature,
which allows a more robust finalization support.

In contrast to old implementation, it doesn't spending linear time , checking what elements became garbage.