#!/usr/bin/env python3
"""
Simple script to test if the webhook can connect to the callback endpoints
"""

import requests
import json
from fastapi import FastAPI, Request
import uvicorn
from threading import Thread
import time

app = FastAPI()

@app.post("/confirmar")
async def confirmar(req: Request):
    body = await req.json()
    print("✅ Confirmação recebida:", body)
    return {"status": "ok"}

@app.post("/cancelar")
async def cancelar(req: Request):
    body = await req.json()
    print("❌ Cancelamento recebido:", body)
    return {"status": "ok"}

def run_server():
    uvicorn.run(app, host="127.0.0.1", port=5000, log_level="info")

if __name__ == "__main__":
    print("Starting callback server on port 5000...")
    server_thread = Thread(target=run_server, daemon=True)
    server_thread.start()
    
    # Keep the server running
    print("Server is running. Press Ctrl+C to stop.")
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        print("\nShutting down...")