from exploit.betterincongito import rbx
from exploit.betterincongito import gyat
import os
from exploit.funcs.instance import Instance
from exploit.mempy.api import Memopy
import ctypes
import requests  # type: ignore
from exploit.funcs.utils import Offsets
import win32gui  # type: ignore
import time
from pyfiglet import figlet_format as ascii
import win32process
from exploit.funcs.utils import GetRenderViewFromLog
from colorama import Fore, Back, Style, init
from datetime import datetime

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
    0x0: f"BetterIncognito is Injected",
    0x1: f"Currently injecting!",
    0x2: f"Failed to find Roblox process.",
    0x3: f"Failed to fetch DataModel :(",
    0x4: f"Failed to fetch certain modules.",
    0x5: f"Roblox terminated while injecting!",
    0x6: f"Failed to find Bridge!"
}

def get_current_time():
    return Fore.LIGHTBLACK_EX + f"{datetime.now().strftime('%Y-%m-%d %H:%M:%S')} " + Style.RESET_ALL

def infolog(message):
    print(f'{Fore.LIGHTGREEN_EX}[BETTERINCOGNITO] {get_current_time()} {Fore.WHITE}{message}')

def bridgelog(message):
    print(f'{Fore.LIGHTYELLOW_EX}[BETTERINCOGNITO] {get_current_time()} {Fore.WHITE}{message}')

def errorlog(message):
    print(f'{Fore.LIGHTRED_EX}[BETTERINCOGNITO] {get_current_time()} {Fore.WHITE}{message}')

if __name__ == "__main__":
    os.system("title BetterIncognito")
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
                    infolog('Waiting for roblox..')
                else:
                    input(infolog('Roblox Opened! Press Enter once you see the roblox window..'))
                    break
        hwnd = win32gui.FindWindow(None, "Roblox")
        _, pid = win32process.GetWindowThreadProcessId(hwnd)
        os.system('cls')
        infolog('Roblox Process Found')
        time.sleep(1)
        infolog(f'Process PID: {pid}')
        infolog(f'Init Script Initalized')
        time.sleep(2)
        infolog(f'Homepage Injection')
        bridgelog('Preparing Bridge')
        time.sleep(2)
        bridgelog('Bridge Initalized')
        time.sleep(3)
        infolog('Please join a game')
        compiled = proto.Inject()
        infolog(errors.get(compiled, "Unknown error"))
