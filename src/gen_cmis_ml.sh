#!/bin/sh


echo "let cmis = ["

for i in $(ls ../docs/asset/stdlib/); do
  echo "\"$i\";"
done

echo "]"
