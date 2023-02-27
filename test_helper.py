#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec 26 15:43:08 2022

@author: caba
"""

import os
import subprocess

if __name__ == '__main__':
    subprocess.run("make build".split())
    tests = os.listdir('test/samples')
    tests = list(filter(lambda f: f.endswith(".mc"), tests))
    cmd = "dune exec _build/default/test/codegen_test.exe -- test/samples/"
    lf, lt = [], []
    green_color = '\033[92m'
    red_color = '\033[91m'
    yellow_color = '\033[93m'
    for f in tests:
        if f.startswith('fail'):
            lf.append(f)
        else:
            lt.append(f)
    lf.sort()
    lt.sort()

    print(f"{yellow_color}" +
          "\n============================ " +
          "Running non-failing tests" +
          " ============================\n")
    for i, test_file in enumerate(lt, 1):
        p = subprocess.run(
            (cmd + test_file).split(),
            capture_output=True,
            text=True)
        if p.stderr:
            print(f"{red_color}File: {test_file} failed but should've passed\n"
                  + p.stderr)
        elif p.stdout.find("succeded!") != -1:  # Testing for segfaults
            print(f"{green_color}File: {test_file} passed ({i}/{len(lt)})")
        else:
            print(f"{red_color}File: {test_file}\n" + p.stdout)

    print(f"{yellow_color}" +
          "\n============================ " +
          "Running failing tests" +
          " ============================\n")
    for i, test_file in enumerate(lf, 1):
        p = subprocess.run(
            (cmd + test_file).split(),
            capture_output=True,
            text=True)
        if p.stderr:
            if p.stderr.find("LLVM ERROR") != -1:
                print(f"{red_color}File: {test_file}\n" + p.stderr)
            else:
                print(f"{green_color}File: {test_file} failed ({i}/{len(lf)})"
                      + yellow_color + p.stderr)
        else:
            if p.stdout.find("Error") != -1:
                print(f"{green_color}File: {test_file} failed ({i}/{len(lf)})"
                      + yellow_color + p.stdout)
            else:
                print(f"{red_color}File: {test_file} passed but should've failed\n")
