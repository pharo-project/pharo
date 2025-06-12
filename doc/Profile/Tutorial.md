# Window Profile Manager Documentation

## Overview
I scan the current windows and build a profile from the presenters or tools inside them. A **profile** is a set of window configurations also called **placeholders**.
I am in the **Windows** menu, named **Profiles**

### Key Concepts
- **Placeholder**: A description of the window on the screen (extent, position, kind, strategy)
- **Strategy**: The way the window will be placed on the screen, such as replace the previous one, stacking, etc.

## Getting Started

To fully understand the functionality, you can:

1. **Create a New Profile**
   - Click on 'New Profile' in the Profiles menu

2. **View Current Profile**
   - Select 'Toggle profile preview' to display visual placeholders of the current profile
   - **Left click** on a placeholder to change the strategy associated with the window
   - **Right click** on a placeholder to remove a registered window from the current profile

## Managing Profiles

### Updating Profiles
- Use 'Update current profile' option to modify your profile without recreating a new one

### Switching Profiles
- When selecting another profile, it becomes the current profile and all current windows on the screen are moved according to placeholders

### Resetting Windows
- Use 'Reset Windows' to reorganize your windows based on your current profile (same behavior as switching profiles)

## Import/Export Functionality

### Exporting Profiles
- With 'Export Profile', the current profile will be saved as a STON file in the Preferences folder
- File format: `Profile:YourProfileName.ston`

### Importing Profiles
- Any profile can be loaded with 'Import Profile'
- **Important**: Make sure your file has 'Profile' in its name to work properly

## Startup Configuration

A profile can be loaded in the StartUp Preferences files by adding this code:

```smalltalk
StartupAction
    name: 'Load Profile'
    code: [
        CavroisWindowManager current makeProfileCurrent: profileName
    ].
```
