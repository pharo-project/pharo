A GLMTransmission models the connection between multiple origin ports and one destination port. Whenever an origin port changes the value, the corresponding transmissions are triggered by the browser.

The result of triggering a transmission is the setting of the value in the destination port. The transmissionStrategy can add further different semantics to this behavior.

A transmission takes place in a context. The context is started every time a new value is set from outside. Afterwards, the context is preserved internally. This is important for braking possible loops of transmission propagation.

There are two kind of origins for a transmission, active and passive ones. A change of value in an active origin will trigger the transmissions originating from it. A change of value in a passive origin will not trigger the transmission. However, a value in a passive origin is still part of the transmission value.