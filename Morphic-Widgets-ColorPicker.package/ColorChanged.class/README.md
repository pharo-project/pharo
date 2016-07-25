I'm a Announcement used to indicate a color has changed. 

Example:

(ColorSelectorDialogWindow new
                        title: 'Choose a color';
                        open;
                        announcer) on: ColorChanged  do: [:ann|
                                                                        UIManager inform: 'Selected color: ', ann newColor asString].