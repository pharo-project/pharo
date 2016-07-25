*System-Object Events
triggerEvent: anEventSelector
withArguments: anArgumentList

    ^(self actionForEvent: anEventSelector)
        valueWithArguments: anArgumentList