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
    0x0: Fore.GREEN + f"[INFO] {Fore.WHITE} Injecting...",
    0x2: Fore.YELLOW + f"[WARN] {Fore.WHITE} Process not found, waiting for Roblox...",
    0x3: Fore.RED + f"[FAILED] {Fore.WHITE} Failed to fetch DataModel.",
    0x4: Fore.RED + f"[FAILED] {Fore.WHITE} Failed to fetch certain modules.",
    0x5: Fore.RED + f"[FAILED] {Fore.WHITE} Roblox termed while injecting.",
    0x6: Fore.RED + f"[FAILED] {Fore.WHITE} Dead Bridge."
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
                    print(Fore.GREEN + f'[INFO] {Fore.WHITE} Waiting for Roblox...')
                else:
                    input(Fore.GREEN + f'[INFO] {Fore.WHITE} Roblox Opened! Press Enter once you see the Roblox window...')
                    break

        os.system('cls')
        print(Fore.GREEN + f'[INFO] {Fore.WHITE} Process found')
        time.sleep(2)

        hwnd = win32gui.FindWindow(None, "Roblox")
        _, pid = win32process.GetWindowThreadProcessId(hwnd)
        print(Fore.GREEN + f'[INFO] {Fore.WHITE} Process PID: {pid}')
        print(Fore.GREEN + f'[INFO] {Fore.WHITE} Injecting method: Homepage')
        time.sleep(2)
        print(Fore.YELLOW + f'[BRIDGE] {Fore.WHITE} Bridge initialized')
        print(Fore.GREEN + f'[INFO] {Fore.WHITE} Injecting...')
        time.sleep(5)
        print(Fore.GREEN + f'[INFO] {Fore.WHITE} Please join a game.')
        compiled = proto.Inject()
        print(errors.get(compiled, "Unknown error"))
