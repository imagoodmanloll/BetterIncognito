from exploit.betterincongito import rbx
from exploit.betterincongito import gyat
import os
from exploit.funcs.instance import Instance
from exploit.mempy.api import Memopy
import ctypes
import requests # type: ignore
from exploit.funcs.utils import Offsets
import win32gui # type: ignore
import time
from pyfiglet import figlet_format as ascii
import win32process
from exploit.funcs.utils import GetRenderViewFromLog
from colorama import Fore, Back, Style, init

main_dir = os.path.dirname(os.path.abspath(__file__))
autoexec_path = os.path.join(main_dir, "autoexec")
Window = None
Process = Memopy(0)
RenderView = GetRenderViewFromLog()
global proto
proto = gyat()
proto.SetAutoExecPath(autoexec_path)
plrname = None
errors = {
    0x0: Fore.GREEN + f"[INFO] {Fore.WHITE} Ready to execute scripts.",
    0x1: "Currently injecting!",
    0x2: "Failed to find Roblox process.",
    0x3: "Failed to fetch DataModel :(",
    0x4: "Failed to fetch certain modules.",
    0x5: "Roblox terminated while injecting!",
    0x6: "Failed to find Bridge!"
}

if __name__ == "__main__":
    os.system("title sigma injector")
    os.system("@echo off")
    os.system("cls")
    jk = 2
    if jk == 1:
        exit(0)
    else:
            if not win32gui.FindWindow(None, "Roblox"):
             while True:
                 time.sleep(2)
                 if not win32gui.FindWindow(None, "Roblox"):
                    print(Fore.GREEN + f'[INFO] {Fore.WHITE} Waiting for roblox..')
                 else:
                      input(Fore.GREEN + f'[INFO] {Fore.WHITE} Roblox Opened! Press Enter once you see the roblox window..')
                      break
            os.system('cls')
            print(Fore.GREEN + f'[INFO] {Fore.WHITE} Process found')
            time.sleep(2)
            print(Fore.GREEN + f'[INFO] {Fore.WHITE} Got PID')
            print(Fore.GREEN + f'[INFO] {Fore.WHITE} Got SexyModel')
            print(Fore.GREEN + f'[INFO] {Fore.WHITE} Injecting method: Homepage')
            print(Fore.GREEN + f'[INFO] {Fore.WHITE} Got LocalPlayer')
            print(Fore.GREEN + f'[INFO] {Fore.WHITE} Found Workspace Folder')
            print(Fore.GREEN + f'[INFO] {Fore.WHITE} Found Autoexecute Folder')
            time.sleep(5)
            print(Fore.GREEN + f'[INFO] {Fore.WHITE} Check passed, injecting... (might take around 5 - 10 seconds)')
            time.sleep(10)
            print(Fore.GREEN + f'[INFO] {Fore.WHITE} Please join a game.')
            compiled = proto.Inject()
            print(errors.get(compiled, "Unknown error"))
