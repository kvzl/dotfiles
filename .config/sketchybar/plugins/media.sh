#!/bin/bash
STATE="$(echo "$INFO" | jq -r '.state')"
APP="$(echo "$INFO" | jq -r '.app')"

if [ "$STATE" = "playing" ] && [ "$APP" = "Spotify" ]; then
  MEDIA="$(echo "$INFO" | jq -r '.title + " - " + .artist')"
  sketchybar --set $NAME \
             icon="󰝚" \
             icon.color="0xff63cf6b" \
             label="$MEDIA" \
             drawing=on
else
  sketchybar --set $NAME drawing=off
fi
