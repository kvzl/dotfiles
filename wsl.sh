#!/bin/bash

export WIN_HOME=`wslpath $(powershell.exe -c "Write-Host \\$env:USERPROFILE")`
ln -sf $WIN_HOME/.kube ~/.kube
ln -sf $WIN_HOME/.aws ~/.aws

