# org-excalidraw

## Introduction

`org-excalidraw` is a package for managing [excalidraw](https://www.excalidraw.com) drawings in org-mode.

It supports creating and linking to a new excalidraw file from an org-mode file


## Requirements

`org-excalidraw` has only been tested against Chromium based browsers.

Excalidraw is a web application. `org-excalidraw` takes advantage of the File Handling API to register Excalidraw as a file handler for `.excalidraw` files.


## Installation

`org-excalidraw` requires external dependencies outside of Emacs and `org-mode`.

1. We need to programmatically export excalidraw files to `.svg` for display.
org-excalidraw depends on [excalidraw_export](https://github.com/Timmmm/excalidraw_export) to do this. It must be available on your system's PATH.

2. To correctly display the produced SVGs, your system needs some fonts installed. 
[The excalidraw_export repo provides these as well](https://github.com/Timmmm/excalidraw_export/tree/master/src).

3. Install [excalidraw](https://www.excalidraw.com) as a PWA using Chrome.

4. Enable the `File Handling API` flag in Chrome. Visit `chrome://flags` to do this.

5. Install and configure `org-excalidraw`.
