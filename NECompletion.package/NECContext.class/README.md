A completion is started by the ECController. The controller creates me to compute the context of the completion. The most important information about the context are the receiverClass and the completionToken. I create a ECModel or subclass when requested by the 'model' method.

I use SHParser and SHRange to parse the text input.