I am ZnBufferedReadStream.

I wrap another ReadStream and add efficient buffering for the typical access pattern of parsers: sending lots of #next, #peek and #atEnd messages.

By design I do not implement #position and #position: or anything based on that.

I can wrap both binary or character streams and act accordingly.

Part of Zinc HTTP Components.